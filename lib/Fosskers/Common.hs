{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module Fosskers.Common where

import           ClassyPrelude
import           Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Hourglass (getWeekDay)
import qualified Data.Text as T
import           Lucid
import           Servant.API
import           Servant.HTML.Lucid
import           Servant.XML
import           Text.Printf
import           Time.Types
import           Xmlbf

---

type JsonAPI = "posts" :> Get '[JSON] [Blog]

type API = JsonAPI
  :<|> "blog" :> Raw
  :<|> "rss-en" :> Get '[XML] Blogs
  :<|> "rss-jp" :> Get '[XML] Blogs
  :<|> "assets" :> Raw
  :<|> "webfonts" :> Raw
  :<|> Get '[HTML] (Html ())

newtype Title = Title Text deriving (Eq, Show, Generic, ToJSON)

-- Evil evil orphan instances.
deriving instance Generic Date
deriving instance ToJSON Date
deriving instance Generic Month
deriving instance ToJSON Month

data Language = English | Japanese deriving (Eq, Ord, Show)

newtype Path = Path Text deriving (Generic, ToJSON)

pathLang :: Path -> Maybe Language
pathLang (Path p) = case T.takeEnd 3 p of
  "-en" -> Just English
  "-jp" -> Just Japanese
  _     -> Nothing

data Blog = Blog { title    :: Title
                 , date     :: Date
                 , filename :: Path
                 , freqs    :: [(Text, Int)] } deriving (Generic, ToJSON)

instance ToXml Blog where
  toXml (Blog (Title t) d (Path p) _) = [b]
    where b = element' "item" mempty
              [ element' "title" mempty [ text t ]
              , element' "link" mempty [ text $ "http://fosskers.ca/blog/" <> p <> ".html" ]
              , element' "pubDate" mempty [ text . pack $ dtt d ]
              , element' "description" mempty [ text t ]]

-- | Format a `Date` in a way acceptable to RSS feeds.
dtt :: Date -> [Char]
dtt d@(Date ye mo da) = printf "%s, %d %s %d 00:00:00 GMT" wd da mo' ye
  where wd :: [Char]
        wd = take 3 . show $ getWeekDay d
        mo' :: [Char]
        mo' = take 3 $ show mo

newtype Blogs = Blogs [Blog]

instance ToXml Blogs where
  toXml (Blogs bs) = [ element' "rss" (HM.singleton "version" "2.0")
                       [ element' "channel" mempty (info <> (bs >>= toXml)) ]]
    where info = [ element' "title" mempty [ text "Fosskers.ca Blog" ]
                 , element' "link" mempty [ text "http://fosskers.ca" ]
                 , element' "description" mempty [ text "Articles on Haskell, Functional Programming, and Japanese" ]
                 ]
