{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Common where

import Data.Aeson (ToJSON)
import Protolude
import Servant.API

---

type API = "posts" :> Get '[JSON] [Blog]

data Blog = Blog { title :: Text, freqs :: [(Text, Int)] } deriving (Generic, ToJSON)
