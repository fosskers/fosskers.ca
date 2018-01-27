{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TupleSections, ViewPatterns #-}

module Main ( main ) where

import           Control.Arrow ((&&&))
import           Data.Char (isAlpha)
import           Data.Default (def)
import qualified Data.HashMap.Strict as M
import           Data.Proxy
import qualified Data.Text as T
import           Filesystem.Path (basename)
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Protolude
import           Servant.API
import           Servant.Server
import           Shelly (ls, readfile, toTextIgnore, shelly)
import           Text.Blaze.Html (Html)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Readers.Org (readOrg)
import           Text.Pandoc.Writers.HTML (writeHtml5)
import           Types

---

newtype Args = Args { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic, ParseRecord)

newtype Env = Env { posts :: M.HashMap Text (M.HashMap Text Int, Html) }

server :: Env -> Server API
server env = pure (blogs env)
  :<|> (\f -> maybe (throwError err404) (pure . snd) . M.lookup f $ posts env)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

-- | Attempt to convert an ORG file to HTML.
html :: Text -> Maybe Html
html t = either (const Nothing) Just $ runPure h
  where h = readOrg def t >>= writeHtml5 def

-- | A mapping of word frequencies.
freq :: Text -> M.HashMap Text Int
freq = M.fromList . map ((maybe "死毒殺悪厄魔" identity . head) &&& length) . group . sort . map T.toLower . filter p . T.words . T.map f
  where f c = bool ' ' c $ isAlpha c
        p (T.length -> l) = l > 2 && l < 13

blogs :: Env -> [Blog]
blogs = map (\(t, (m, _)) -> Blog t m) . M.toList . posts

main :: IO ()
main = do
  Args (Helpful p) <- getRecord "Backend server for fosskers.ca"
  ps <- shelly (ls "blog" >>= traverse (\f -> (toTextIgnore $ basename f,) <$> readfile f))
  let prt = maybe 8081 identity p
      ps' = M.fromList . catMaybes $ map (traverse (sequenceA . (freq &&& html))) ps
  putText $ "Running on port " <> show prt
  W.run prt . app $ Env ps'
