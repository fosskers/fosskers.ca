{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

module Main ( main ) where

import           Data.Default (def)
import qualified Data.Map.Strict as M
import           Data.Proxy
import           Filesystem.Path (basename)
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Protolude
import           Servant.API
import           Servant.HTML.Blaze
import           Servant.Server
import           Shelly (ls, readfile, toTextIgnore, shelly)
import           Text.Blaze.Html (Html)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Readers.Org (readOrg)
import           Text.Pandoc.Writers.HTML (writeHtml5)

---

data Args = Args { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic, ParseRecord)

data Env = Env { posts :: M.Map Text Html }

type API = "posts" :> Get '[JSON] [Text]
  :<|> "posts" :> Capture "post" Text :> Get '[HTML] Html

server :: Env -> Server API
server env = pure (M.keys $ posts env)
  :<|> (\f -> maybe (throwError err404) pure . M.lookup f $ posts env)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

-- | Attempt to convert an ORG file to HTML.
html :: Text -> Maybe Html
html t = either (const Nothing) Just $ runPure h
  where h = readOrg def t >>= writeHtml5 def

main :: IO ()
main = do
  Args (Helpful p) <- getRecord "Backend server for fosskers.ca"
  ps <- shelly (ls "blog" >>= traverse (\f -> (toTextIgnore $ basename f,) <$> readfile f))
  let prt = maybe 8081 identity p
      ps' = M.fromList . catMaybes $ map (traverse html) ps
  putText $ "Running on port " <> show prt
  W.run prt . app $ Env ps'
