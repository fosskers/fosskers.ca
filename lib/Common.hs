{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Common where

import Data.Aeson (ToJSON)
import Protolude
import Servant.API
import Time.Types

---

type API = "posts" :> Get '[JSON] [Blog]

newtype Title = Title Text deriving (Eq, Show, Generic, ToJSON)

-- Evil evil orphan instances.
deriving instance Generic Date
deriving instance ToJSON Date
deriving instance Generic Month
deriving instance ToJSON Month

newtype Path = Path Text deriving (Generic, ToJSON)

data Blog = Blog { engTitle :: Title
                 , japTitle :: Title
                 , date     :: Date
                 , filename :: Path
                 , freqs    :: [(Text, Int)] } deriving (Generic, ToJSON)
