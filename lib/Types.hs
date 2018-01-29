{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import           Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as M
import           Protolude
import           Servant.API

---

type API = "posts" :> Get '[JSON] [Blog]

data Blog = Blog { title :: Text, freqs :: M.HashMap Text Int } deriving (Generic, ToJSON)
