{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import           Control.Concurrent (getNumCapabilities)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Bifunctor (first)
import           Data.Bitraversable (bitraverse)
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Org as O
import qualified Data.Org.Lucid as O
import           Data.These (These(..), partitionEithersNE)
import           Fosskers.Common
import           Fosskers.Site (Page(..), site)
import           Fosskers.Site.About (about)
import           Fosskers.Site.Blog (blog, choose, newest)
import           Fosskers.Site.CV (cv)
import           Lucid
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Applicative hiding (style)
import           RIO hiding (first)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T
import           Servant.API
import           Servant.Server
import           Servant.Server.StaticFiles (serveDirectoryFileServer)
import           Shelly hiding (path)
import           Skylighting hiding (formatHtmlBlock)
import           Skylighting.Format.HTML.Lucid (formatHtmlBlock)
import           System.Environment (lookupEnv)
import           Text.Megaparsec (errorBundlePretty, parse)
import           UnliftIO.Directory (doesFileExist)

---

newtype Args = Args (Maybe Int)

pArgs :: Parser Args
pArgs = Args
  <$> optional (option auto $ long "port" <> help "Port to listen on, otherwise $PORT")

newtype Env = Env LogFunc
  deriving stock (Generic)

instance HasLogFunc Env where
  logFuncL f (Env lf) = Env <$> f lf

server :: Pages -> Blogs -> Server API
server ps bs =
  serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> (\l -> pure . site l About $ about ps l)
  :<|> (\l -> pure . site l CV $ cv ps l)
  :<|> (\l -> pure . site l Posts . blog bs l . Just $ newest bs l)
  :<|> (\l t -> pure . site l Posts . blog bs l $ choose bs l t)
  :<|> pure . rss bs
  :<|> (\l -> pure . site l Posts . blog bs l . Just $ newest bs l)
  :<|> pure (site English Posts . blog bs English . Just $ newest bs English)

rss :: Blogs -> Language -> ByLanguage
rss bs l = ByLanguage $ NEL.sortWith (Down . orgDate . blogRaw) ps
  where
    ps = case l of
      English  -> engSorted bs
      Japanese -> japSorted bs

app :: Pages -> Blogs -> Application
app ps bs = gzip (def { gzipFiles = GzipCompress })
  . serve (Proxy :: Proxy API)
  $ server ps bs

-- | Syntax highlighting "middleware" thanks to Skylighting and org-mode-lucid.
skylighting :: O.Highlighting
skylighting l t = maybe (O.codeHTML l t) (formatHtmlBlock fo) $ do
  O.Language lang <- l <|> Just (O.Language "Default")
  syn <- syntaxByName defaultSyntaxMap lang
  hush $ tokenize tc syn t
  where
    tc = TokenizerConfig defaultSyntaxMap False
    fo = defaultFormatOpts
      { containerClasses = "src" : maybe [] (\(O.Language l') -> ["src-" <> l']) l }

-- | Abosolute paths to all the @.org@ blog files.
orgFiles :: Sh [FilePath]
orgFiles = do
  cd "blog"
  filter (L.isSuffixOf ".org") <$> (ls "." >>= traverse absPath)

-- | Render all ORG files to HTML.
orgs :: NonEmpty FilePath -> RIO e (These (NonEmpty Text) (NonEmpty Blog))
orgs = fmap partitionEithersNE . traverse g
  where
    style = O.OrgStyle True (Just $ O.TOC "Contents" 2) True skylighting (Just ' ')

    g :: FilePath -> RIO e (Either Text Blog)
    g f = do
      content <- eread f
      let !path = T.pack f
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile f c
        lang <- note ("Invalid language given for file: " <> path) $ pathLang f
        void . note ("No date provided for: " <> path) $ orgDate ofile
        void . note ("No title provided for: " <> path) . M.lookup "TITLE" $ O.orgMeta ofile
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
    astyle = O.OrgStyle False Nothing False skylighting (Just ' ')
    cstyle = O.OrgStyle True (Just $ O.TOC "Index" 2) True skylighting (Just ' ')
    jstyle = O.OrgStyle True (Just $ O.TOC "目次" 2) True skylighting (Just ' ')

    f :: O.OrgStyle -> FilePath -> MaybeT IO (Html ())
    f s fp = O.body s <$> MaybeT (orgd fp)

orgd :: FilePath -> IO (Maybe O.OrgFile)
orgd fp = (hush >=> O.org) <$> eread fp

main :: IO ()
main = do
  args <- execParser opts
  lopt <- setLogUseLoc False <$> logOptionsHandle stderr True
  withLogFunc lopt $ \logFunc -> runRIO (Env logFunc) (setup args)
  where
    opts = info (pArgs <**> helper) (fullDesc <> header "Server for fosskers.ca")

setup :: Args -> RIO Env ()
setup args = liftIO pages >>= \case
  Nothing -> logError "Couldn't read static pages."
  Just ps -> fmap NEL.nonEmpty (shelly orgFiles) >>= \case
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
work (Args p) ps ens jps = do
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
sortByDate = NEL.reverse . NEL.sortWith (orgDate . blogRaw)

mapify :: NonEmpty Blog -> NEMap Text Blog
mapify = NEM.fromList . NEL.map (blogSlug &&& id)
