{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import Data.Bitraversable (bitraverse)
import           Control.Concurrent (getNumCapabilities)
import           Control.Error.Util (note)
import           Data.Bifunctor (first)
import           Data.Generics.Product (typed)
import qualified Data.Org as O
import qualified Data.Org.Lucid as O
import           Data.These (These(..), partitionEithersNE)
import           Fosskers.Common
import           Fosskers.Site (Page(..), site)
import           Fosskers.Site.About (about)
import           Fosskers.Site.Blog (blog, newest)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Generic
import           RIO hiding (first)
import           System.Directory (doesFileExist)
-- import qualified RIO.Map as M
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

data Env = Env
  { engPosts :: !(NonEmpty Blog)
  , japPosts :: !(NonEmpty Blog)
  , logF     :: !LogFunc }
  deriving stock (Generic)

instance HasLogFunc Env where
  logFuncL = typed @LogFunc

server :: Env -> Server API
server env =
  serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> (\l -> pure . site l About $ about l)
  :<|> (\l -> pure . site l Posts $ newest ens jps l)
  :<|> (\l t -> pure . site l Posts $ blog ens jps l t)
  -- :<|> pure . rss (stats env)
  :<|> (\l -> pure . site l Posts $ newest ens jps l)
  :<|> pure (site English Posts $ newest ens jps English)
  where
    ens = engPosts env
    jps = japPosts env

-- TODO What type issues?
-- | Split off from `server` to avoid type issues.
-- jsonServer :: Env -> Server JsonAPI
-- jsonServer env = pure (stats env)
--   :<|> pure . analysis
--   :<|> (\t -> pure . M.lookup t $ texts env)

-- rss :: [Blog] -> Language -> Blogs
-- rss bs l = Blogs . L.sortOn (Down . date) $ filter (\b -> pathLang (filename b) == Just l) bs

app :: Env -> Application
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
orgs :: NonEmpty FilePath -> IO (These (NonEmpty Text) (NonEmpty Blog))
orgs = fmap partitionEithersNE . traverse g
  where
    g :: FilePath -> IO (Either Text Blog)
    g f = do
      content <- eread f
      let !path = T.pack f
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile f c
        lang <- note ("Invalid language given for file: " <> path) $ pathLang path
        Right . Blog lang ofile $ O.body ofile

eread :: FilePath -> IO (Either Text Text)
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
  fmap NEL.nonEmpty (shelly orgFiles) >>= \case
    Nothing -> exitFailure
    Just fs -> orgs fs >>= \case
      This _ -> exitFailure
      That bs -> maybe exitFailure (uncurry $ work args) $ splitLs bs
      These _ bs -> do
        -- traverse_ T.putStrLn errs
        maybe exitFailure (uncurry $ work args) $ splitLs bs

splitLs :: NonEmpty Blog -> Maybe (NonEmpty Blog, NonEmpty Blog)
splitLs = bitraverse NEL.nonEmpty NEL.nonEmpty . NEL.partition (\b -> blogLang b == English)

work :: Args -> NonEmpty Blog -> NonEmpty Blog -> IO ()
work (Args (Helpful p)) ens jps = do
  lopt <- setLogUseLoc False <$> logOptionsHandle stderr True
  withLogFunc lopt $ \logFunc -> do
    -- afs <- analysisFiles
    herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
    cores <- getNumCapabilities
    let !prt = fromMaybe 8081 $ p <|> herokuPort
        !env = Env (sortByDate ens) (sortByDate jps) logFunc
    runRIO env $ do
      -- traverse_ (logWarn . display) errs
      -- logInfo $ "Analysis files read: " <> display (length afs)
      logInfo $ "Blog posts read: " <> display (length ens + length jps)
      logInfo $ "Listening on port " <> display prt <> " with " <> display cores <> " cores"
      liftIO . W.run prt $ app env

sortByDate :: NonEmpty Blog -> NonEmpty Blog
sortByDate = NEL.sortWith (O.metaDate . O.orgMeta . blogRaw)
