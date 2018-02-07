{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Common where

import Data.Aeson (ToJSON)
import Protolude
import Servant.API

---

type API = "posts" :> Get '[JSON] [Blog]

newtype Title = Title Text deriving (Generic, ToJSON)

newtype Date = Date Text deriving (Generic, ToJSON)

newtype Path = Path Text deriving (Generic, ToJSON)

data Blog = Blog { engTitle :: Title
                 , japTitle :: Title
                 , date     :: Date
                 , filename :: Path
                 , freqs    :: [(Text, Int)] } deriving (Generic, ToJSON)
