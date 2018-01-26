{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main ( main ) where

import           Data.Proxy
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Protolude
import           Servant.API
import           Servant.Server

---

data Env = Env { port :: Maybe Int <?> "Port to listen for requests on." } deriving (Generic)

instance ParseRecord Env

type API = "posts" :> Get '[JSON] [Text]

server :: Server API
server = pure []

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  Env (Helpful p) <- getRecord "Backend server for fosskers.ca"
  W.run (maybe 8081 identity p) app
