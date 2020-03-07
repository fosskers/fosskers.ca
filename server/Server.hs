{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bitraversable (bitraverse)
import Lucid
import           Data.Map.NonEmpty (NEMap)
import           Control.Concurrent (getNumCapabilities)
import           Control.Error.Util (note, hush)
import           Data.Bifunctor (first)
import           Data.Generics.Product (typed)
import qualified Data.Org as O
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Org.Lucid as O
import           Data.These (These(..), partitionEithersNE)
import           Fosskers.Common
import           Fosskers.Site (Page(..), site)
import           Fosskers.Site.About (about)
import           Fosskers.Site.CV (cv)
import           Fosskers.Site.Blog (choose, newest, blog)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Generic
import           RIO hiding (first)
import           UnliftIO.Directory (doesFileExist)
import qualified RIO.List as L
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles (serveDirectoryFileServer)
import           Shelly hiding (path)
import           System.Environment (lookupEnv)
import           Text.Megaparsec (errorBundlePretty, parse)

---

newtype Args = Args
  { port :: Maybe Int <?> "Port to listen for requests on, otherwise $PORT" }
  deriving stock (Generic)
  deriving anyclass (ParseRecord)

newtype Env = Env LogFunc
  deriving stock (Generic)

instance HasLogFunc Env where
  logFuncL = typed @LogFunc

server :: Pages -> Blogs -> Server API
server ps bs =
  serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> (\l -> pure . site l About $ about ps l)
  :<|> (\l -> pure . site l CV $ cv ps l)
  :<|> (\l -> pure . site l Posts . blog bs l $ newest bs l)
  :<|> (\l t -> pure . site l Posts . blog bs l $ choose bs l t)
  :<|> pure . rss bs
  :<|> (\l -> pure . site l Posts . blog bs l $ newest bs l)
  :<|> pure (site English Posts . blog bs English $ newest bs English)

rss :: Blogs -> Language -> ByLanguage
rss bs l = ByLanguage $ NEL.sortWith (Down . O.metaDate . O.orgMeta . blogRaw) ps
  where
    ps = case l of
      English -> engSorted bs
      Japanese -> japSorted bs

app :: Pages -> Blogs -> Application
app ps bs = gzip (def { gzipFiles = GzipCompress })
  . serve (Proxy :: Proxy API)
  $ server ps bs

-- | Abosolute paths to all the @.org@ blog files.
orgFiles :: Sh [FilePath]
orgFiles = do
  cd "blog"
  filter (L.isSuffixOf ".org") <$> (ls "." >>= traverse absPath)

-- | Render all ORG files to HTML.
orgs :: MonadIO m => NonEmpty FilePath -> m (These (NonEmpty Text) (NonEmpty Blog))
orgs = fmap partitionEithersNE . traverse g
  where
    style = O.OrgStyle True (Just $ O.TOC "Contents" 2) True O.codeHTML

    -- g :: FilePath -> m (Either Text Blog)
    g f = do
      content <- eread f
      let !path = T.pack f
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile f c
        lang <- note ("Invalid language given for file: " <> path) $ pathLang f
        void . note ("No date provided for: " <> path) . O.metaDate $ O.orgMeta ofile
        void . note ("No title provided for: " <> path) . O.metaTitle $ O.orgMeta ofile
        Right . Blog lang (pathSlug f) ofile $ O.body style ofile

eread :: MonadIO m => FilePath -> m (Either Text Text)
eread path = do
  exists <- doesFileExist path
  if exists
     then Right <$> readFileUtf8 path
     else pure . Left $ T.pack path <> " doesn't exist to be read."

pages :: IO (Maybe Pages)
pages = runMaybeT $ Pages
  <$> f astyle "org/about-en.org"
  <*> f astyle "org/about-jp.org"
  <*> f cstyle "org/cv-en.org"
  <*> f jstyle "org/cv-jp.org"
  where
    astyle = O.OrgStyle False Nothing False O.codeHTML
    cstyle = O.OrgStyle True (Just $ O.TOC "Index" 2) True O.codeHTML
    jstyle = O.OrgStyle True (Just $ O.TOC "目次" 2) True O.codeHTML

    f :: O.OrgStyle -> FilePath -> MaybeT IO (Html ())
    f s fp = O.body s <$> MaybeT (orgd fp)

orgd :: FilePath -> IO (Maybe O.OrgFile)
orgd fp = (hush >=> O.org) <$> eread fp

main :: IO ()
main = do
  args <- getRecord "Backend server for fosskers.ca"
  lopt <- setLogUseLoc False <$> logOptionsHandle stderr True
  withLogFunc lopt $ \logFunc -> runRIO (Env logFunc) (setup args)

setup :: Args -> RIO Env ()
setup args = do
  liftIO pages >>= \case
    Nothing -> logError "Couldn't read static pages."
    Just ps -> do
      fmap NEL.nonEmpty (shelly orgFiles) >>= \case
        Nothing -> logError "No .org files were found." >> exitFailure
        Just fs -> orgs fs >>= \case
          This errs -> traverse_ (logWarn . display) errs >> exitFailure
          That bs -> case splitLs bs of
            Nothing -> logError "Missing posts from Japanese or English" >> exitFailure
            Just r  -> uncurry (work args ps) r
          These errs bs -> do
            traverse_ (logWarn . display) errs
            maybe exitFailure (uncurry $ work args ps) $ splitLs bs

splitLs :: NonEmpty Blog -> Maybe (NonEmpty Blog, NonEmpty Blog)
splitLs = bitraverse NEL.nonEmpty NEL.nonEmpty . NEL.partition (\b -> blogLang b == English)

work :: Args -> Pages -> NonEmpty Blog -> NonEmpty Blog -> RIO Env ()
work (Args (Helpful p)) ps ens jps = do
  -- afs <- analysisFiles
  herokuPort <- (>>= readMaybe) <$> liftIO (lookupEnv "PORT")
  cores <- liftIO getNumCapabilities
  let !prt = fromMaybe 8081 $ p <|> herokuPort
      !eng = mapify ens
      !jap = mapify jps
      !bls = Blogs (sortByDate ens) (sortByDate jps) eng jap
  -- logInfo $ "Analysis files read: " <> display (length afs)
  logInfo $ "Blog posts read: " <> display (length ens + length jps)
  logInfo $ "Listening on port " <> display prt <> " with " <> display cores <> " cores"
  liftIO . W.run prt $ app ps bls

sortByDate :: NonEmpty Blog -> NonEmpty Blog
sortByDate = NEL.reverse . NEL.sortWith (O.metaDate . O.orgMeta . blogRaw)

mapify :: NonEmpty Blog -> NEMap Text Blog
mapify = NEM.fromList . NEL.map (blogSlug &&& id)
