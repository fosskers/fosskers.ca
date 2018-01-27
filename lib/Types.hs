{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import           Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as M
import           Protolude
import           Text.Blaze.Html (Html)
import           Servant.API
import           Servant.HTML.Blaze

---

type API = "posts" :> Get '[JSON] [Blog]
  :<|> "posts" :> Capture "post" Text :> Get '[HTML] Html

data Blog = Blog { title :: Text, freqs :: M.HashMap Text Int } deriving (Generic, ToJSON)
