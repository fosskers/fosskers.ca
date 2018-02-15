{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Common where

import           Data.Aeson (ToJSON)
import           Lucid
import           Protolude
import           Servant.API
import qualified Servant.HTML.Lucid as SHL
import qualified Servant.HTML.Blaze as SHB
import qualified Text.Blaze.Html as B
import           Time.Types

---

type JsonAPI = "posts" :> Get '[JSON] [Blog]

type API = JsonAPI
  :<|> "blog" :> Capture "post" Text :> Get '[SHB.HTML] B.Html
  :<|> "assets" :> Raw
  :<|> "webfonts" :> Raw
  :<|> Get '[SHL.HTML] (Html ())

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
