{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

module Main ( main ) where

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

---

data Args = Args { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic, ParseRecord)

data Env = Env { posts :: M.Map Text Text }

type API = "posts" :> Get '[JSON] [Text]
  :<|> "posts" :> Capture "post" Text :> Get '[HTML] Text

server :: Env -> Server API
server env = pure (M.keys $ posts env)
  :<|> (\f -> maybe (throwError err404) pure . M.lookup f $ posts env)

app :: Env -> Application
app = serve (Proxy :: Proxy API) . server

main :: IO ()
main = do
  ps <- M.fromList <$> shelly (ls "blog" >>= traverse (\f -> (toTextIgnore $ basename f,) <$> readfile f))
  Args (Helpful p) <- getRecord "Backend server for fosskers.ca"
  let prt = maybe 8081 identity p
  putText $ "Running on port " <> show prt
  W.run prt . app $ Env ps
