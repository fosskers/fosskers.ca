{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import           Control.Concurrent (getNumCapabilities)
import           Data.Bifunctor (first)
import           Data.Generics.Product.Fields (field)
import qualified Data.Org as O
import qualified Data.Org.Lucid as O
import           Fosskers.Common
import           Fosskers.Site (Page(..), site)
import           Fosskers.Site.About (about)
import           Fosskers.Site.Blog (blog, newest)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.Gzip
import           Options.Generic
import           RIO hiding (first)
import qualified RIO.Map as M
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
  { stats :: ![Blog]
  , logF  :: !LogFunc }
  deriving stock (Generic)

instance HasLogFunc Env where
  logFuncL = field @"logF"

server :: Env -> Server API
server env =
  serveDirectoryFileServer "assets"
  :<|> serveDirectoryFileServer "assets/webfonts"
  :<|> (\l -> pure . site l About $ about l)
  :<|> (\l -> pure . site l Posts $ newest l)
  :<|> (\l t -> pure . site l Posts $ blog l t)
  -- :<|> pure . rss (stats env)
  :<|> (\l -> pure . site l Posts $ newest l)
  :<|> pure (site English Posts $ newest English)

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

-- | Render all ORG files to HTML, also yielding word frequencies for each file.
--
-- Can assume:
--   - Every English article has a Japanese analogue
--   - The true title always appears on the first line of the file
--   - The original (ballpark) date of writing is on the second line of the "base" file
orgs :: Sh ([Text], [Blog])
orgs = do
  cd "blog"
  files <- filter (T.isSuffixOf ".org") <$> lsT "."
  vs <- traverse g files
  pure $ partitionEithers vs
  where
    g :: Text -> Sh (Either Text Blog)
    g f = do
      let path = T.unpack f
      content <- eread path
      pure $ do
        c <- content
        ofile <- first (T.pack . errorBundlePretty) $ parse O.orgFile path c
        pure . Blog ofile $ O.body ofile

-- TODO I don't like the way this feels/looks
eread :: FilePath -> Sh (Either Text Text)
eread path = do
  exists <- test_f path
  if exists
     then Right <$> readfile path
     else pure . Left $ toTextIgnore path <> " doesn't exist to be read"

analysisFiles :: IO (Map Text Text)
analysisFiles = fmap (M.fromList . rights) . shelly $ traverse f files
  where
    f :: Text -> Sh (Either Text (Text, Text))
    f fp = fmap ((fp,) <$>) . eread $ fromText ("server/" <> fp <> ".txt")
    files = [ "doraemon", "rashomon", "iamacat", "sumo" ]

main :: IO ()
main = do
  Args (Helpful p) <- getRecord "Backend server for fosskers.ca"
  lopts <- setLogMinLevel LevelInfo . setLogUseLoc False <$> logOptionsHandle stderr True
  withLogFunc lopts $ \logFunc -> do
    (errs, ps) <- shelly orgs
    afs <- analysisFiles
    herokuPort <- (>>= readMaybe) <$> lookupEnv "PORT"
    cores <- getNumCapabilities
    let !prt = fromMaybe 8081 $ p <|> herokuPort
        !env = Env ps logFunc
    runRIO env $ do
      traverse_ (logWarn . display) errs
      logInfo $ "Analysis files read: " <> display (length afs)
      logInfo $ "Listening on port " <> display prt <> " with " <> display cores <> " cores"
      liftIO . W.run prt $ app env
