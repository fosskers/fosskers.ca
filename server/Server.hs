{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import Data.Bitraversable (bitraverse)
import           Data.Map.NonEmpty (NEMap)
import           Control.Concurrent (getNumCapabilities)
import           Control.Error.Util (note)
import           Data.Bifunctor (first)
import           Data.Generics.Product (typed)
import qualified Data.Org as O
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Org.Lucid as O
import           Data.These (These(..), partitionEithersNE)
import           Fosskers.Common
import           Fosskers.Site (Page(..), site)
import           Fosskers.Site.About (about)
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

server :: Blogs -> Server API
server bs =
  serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> (\l -> pure . site l About $ about l)
  :<|> (\l -> pure . site l Posts . blog bs l $ newest bs l)
  :<|> (\l t -> pure . site l Posts . blog bs l $ choose bs l t)
  -- :<|> pure . rss (stats env)
  :<|> (\l -> pure . site l Posts . blog bs l $ newest bs l)
  :<|> pure (site English Posts . blog bs English $ newest bs English)

-- TODO What type issues?
-- | Split off from `server` to avoid type issues.
-- jsonServer :: Env -> Server JsonAPI
-- jsonServer env = pure (stats env)
--   :<|> pure . analysis
--   :<|> (\t -> pure . M.lookup t $ texts env)

-- rss :: [Blog] -> Language -> Blogs
-- rss bs l = Blogs . L.sortOn (Down . date) $ filter (\b -> pathLang (filename b) == Just l) bs

app :: Blogs -> Application
app = gzip (def { gzipFiles = GzipCompress }) . serve (Proxy :: Proxy API) . server

-- -- | A mapping of word frequencies.
-- freq :: Text -> [(Text, Int)]
-- freq = map (h &&& length) . L.group . L.sort . filter g . map T.toLower . T.words . T.map f
--   where
--     f :: Char -> Char
--     f c = bool ' ' c $ isAlpha c

--     g :: Text -> Bool
--     g w = let l = T.length w in l > 2 && l < 13 && not (S.member w functionWords)

--     h :: [Text] -> Text
--     h []    = "死毒殺悪厄魔"
--     h (c:_) = c

-- functionWords :: Set Text
-- functionWords = S.fromList
--   [ "and", "but", "for", "our", "the", "that", "this", "these", "those", "then", "than"
--   , "what", "when", "where", "will", "your", "you", "are", "can", "has", "have"
--   , "here", "there", "how", "who", "its", "just", "not", "now", "only", "they" ]

-- | Abosolute paths to all the @.org@ blog files.
orgFiles :: Sh [FilePath]
orgFiles = do
  cd "blog"
  filter (L.isSuffixOf ".org") <$> (ls "." >>= traverse absPath)

-- | Render all ORG files to HTML.
orgs :: MonadIO m => NonEmpty FilePath -> m (These (NonEmpty Text) (NonEmpty Blog))
orgs = fmap partitionEithersNE . traverse g
  where
    -- g :: FilePath -> m (Either Text Blog)
    g f = do
      content <- eread f
      let !path = T.pack f
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile f c
        lang <- note ("Invalid language given for file: " <> path) $ pathLang f
        void . note ("No date provided for: " <> path) . O.metaDate $ O.orgMeta ofile
        Right . Blog lang (pathSlug f) ofile $ O.body ofile

eread :: MonadIO m => FilePath -> m (Either Text Text)
eread path = do
  exists <- doesFileExist path
  if exists
     then Right <$> readFileUtf8 path
     else pure . Left $ T.pack path <> " doesn't exist to be read."

-- analysisFiles :: IO (Map Text Text)
-- analysisFiles = fmap (M.fromList . rights) . shelly $ traverse f files
--   where
--     f :: Text -> Sh (Either Text (Text, Text))
--     f fp = fmap ((fp,) <$>) . eread $ fromText ("server/" <> fp <> ".txt")
--     files = [ "doraemon", "rashomon", "iamacat", "sumo" ]

main :: IO ()
main = do
  args <- getRecord "Backend server for fosskers.ca"
  lopt <- setLogUseLoc False <$> logOptionsHandle stderr True
  withLogFunc lopt $ \logFunc -> runRIO (Env logFunc) (setup args)

setup :: Args -> RIO Env ()
setup args = do
  fmap NEL.nonEmpty (shelly orgFiles) >>= \case
    Nothing -> logError "No .org files were found." >> exitFailure
    Just fs -> orgs fs >>= \case
      This errs -> traverse_ (logWarn . display) errs >> exitFailure
      That bs -> case splitLs bs of
        Nothing -> logError "Missing posts from Japanese or English" >> exitFailure
        Just r  -> uncurry (work args) r
      These errs bs -> do
        traverse_ (logWarn . display) errs
        maybe exitFailure (uncurry $ work args) $ splitLs bs

splitLs :: NonEmpty Blog -> Maybe (NonEmpty Blog, NonEmpty Blog)
splitLs = bitraverse NEL.nonEmpty NEL.nonEmpty . NEL.partition (\b -> blogLang b == English)

work :: Args -> NonEmpty Blog -> NonEmpty Blog -> RIO Env ()
work (Args (Helpful p)) ens jps = do
  -- afs <- analysisFiles
  herokuPort <- (>>= readMaybe) <$> liftIO (lookupEnv "PORT")
  cores <- liftIO getNumCapabilities
  let !prt = fromMaybe 8081 $ p <|> herokuPort
      !eng = mapify ens
      !jap = mapify jps
      !env = Blogs (sortByDate ens) (sortByDate jps) eng jap
  -- logInfo $ "Analysis files read: " <> display (length afs)
  logInfo $ "Blog posts read: " <> display (length ens + length jps)
  logInfo $ "Listening on port " <> display prt <> " with " <> display cores <> " cores"
  liftIO . W.run prt $ app env

sortByDate :: NonEmpty Blog -> NonEmpty Blog
sortByDate = NEL.reverse . NEL.sortWith (O.metaDate . O.orgMeta . blogRaw)

mapify :: NonEmpty Blog -> NEMap Text Blog
mapify = NEM.fromList . NEL.map (blogSlug &&& id)
