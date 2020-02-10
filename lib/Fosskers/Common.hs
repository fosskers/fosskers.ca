{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fosskers.Common
  ( -- * APIs
    API
  , JsonAPI
    -- * Blog Posts
  , Blog(..)
  , Blogs(..)
  , Title(..)
  , Language(..)
  , Path(..)
  , pathLang
  ) where

import           BasePrelude
import           Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Hourglass (getWeekDay)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Fosskers.Kanji (Analysis)
import           Lucid (Html)
import           Servant.API
import           Servant.HTML.Lucid
import           Servant.XML
import           Time.Types (Date(..), Month(..))
import           Xmlbf (Node, ToXml(..), element, text)

---

type JsonAPI = "posts" :> Get '[JSON] [Blog]
  :<|> "kanji" :> ReqBody '[JSON] Text :> Post '[JSON] Analysis
  :<|> "kanji" :> Capture "text" Text :> Get '[JSON] (Maybe Analysis)

type API =
  "assets" :> Raw
  :<|> "rss-en" :> Get '[XML] Blogs
  :<|> "rss-jp" :> Get '[XML] Blogs
  :<|> Capture "language" Language :> "about" :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> "blog" :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> "blog" :> Capture "title" Text :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> Get '[HTML] (Html ())
  :<|> Get '[HTML] (Html ())

newtype Title = Title Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- TODO Use better time types. What does Aeson have support for?
-- Evil evil orphan instances.
deriving stock instance Generic Date
deriving stock instance Generic Month
deriving anyclass instance ToJSON Date
deriving anyclass instance ToJSON Month

data Language = English | Japanese
  deriving stock (Eq, Ord, Show, Generic)

instance FromHttpApiData Language where
  parseUrlPiece "en" = Right English
  parseUrlPiece "jp" = Right Japanese
  parseUrlPiece l    = Left $ "Invalid language: " <> l

newtype Path = Path Text
  deriving stock (Generic)
  deriving anyclass (ToJSON)

pathLang :: Path -> Maybe Language
pathLang (Path p) = case T.takeEnd 3 p of
  "-en" -> Just English
  "-jp" -> Just Japanese
  _     -> Nothing

data Blog = Blog
  { title    :: !Title
  , date     :: !Date
  , filename :: !Path
  , freqs    :: ![(Text, Int)] }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToXml Blog where
  toXml (Blog (Title t) d (Path p) _) = b
    where
      b :: [Node]
      b = element "item" mempty
          $  element "title" mempty (text $ TL.fromStrict t)
          <> element "link" mempty (text . TL.fromStrict $ "https://fosskers.ca/blog/" <> p <> ".html")
          <> element "pubDate" mempty (text . TL.pack $ dtt d)
          <> element "description" mempty (text $ TL.fromStrict t)

-- | Format a `Date` in a way acceptable to RSS feeds.
dtt :: Date -> String
dtt d@(Date ye mo da) = printf "%s, %d %s %d 00:00:00 GMT" wd da mo' ye
  where
    wd = take 3 . show $ getWeekDay d
    mo' = take 3 $ show mo

newtype Blogs = Blogs [Blog]

instance ToXml Blogs where
  toXml (Blogs bs) = element "rss" (HM.singleton "version" "2.0")
                     $ element "channel" mempty (info <> foldMap toXml bs)
    where
      info :: [Node]
      info = element "title" mempty (text "Fosskers.ca Blog")
             <> element "link" mempty (text "https://fosskers.ca")
             <> element "description" mempty
             (text "Articles on Haskell, Functional Programming, and Japanese")
