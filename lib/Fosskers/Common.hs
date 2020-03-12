{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Fosskers.Common
  ( -- * APIs
    API
    -- * Static Pages
  , Pages(..)
    -- * Blog Posts
  , Blog(..)
  , Blogs(..)
  , Title(..)
  , Language(..)
  , langPath
  , Path(..)
  , pathLang
  , pathSlug
  , ByLanguage(..)
  , orgDate
  ) where

import           Data.Aeson (ToJSON)
import           Data.Hourglass (getWeekDay)
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Org as O
import           Data.Time.Calendar (Day(..), fromGregorian)
import           Data.Time.Format
import           Lucid (Html)
import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.Map as M
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import           Servant.API
import           Servant.HTML.Lucid
import           Servant.XML
import           System.FilePath.Posix (takeBaseName)
import           Text.Printf (printf)
import           Time.Compat (dateFromTAIEpoch)
import           Time.Types (Date(..))
import           Xmlbf (Node, ToXml(..), element, text)

---

type API =
  "assets" :> Raw
  :<|> "webfonts" :> Raw
  :<|> Capture "language" Language :> "about" :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> "cv"    :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> "blog" :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> "blog" :> Capture "title" Text :> Get '[HTML] (Html ())
  :<|> Capture "language" Language :> "rss" :> Get '[XML] ByLanguage
  :<|> Capture "language" Language :> Get '[HTML] (Html ())
  :<|> Get '[HTML] (Html ())

newtype Title = Title Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Language = English | Japanese
  deriving stock (Eq, Ord, Show, Generic)

langPath :: Language -> Text
langPath English  = "en"
langPath Japanese = "jp"

instance FromHttpApiData Language where
  parseUrlPiece "en" = Right English
  parseUrlPiece "jp" = Right Japanese
  parseUrlPiece l    = Left $ "Invalid language: " <> l

data Blog = Blog
  { blogLang :: !Language
  , blogSlug :: !Text
  , blogRaw  :: !O.OrgFile
  , blogHtml :: !(Html ()) }

data Blogs = Blogs
  { engSorted :: !(NonEmpty Blog)
  , japSorted :: !(NonEmpty Blog)
  , engPosts  :: !(NEMap Text Blog)
  , japPosts  :: !(NEMap Text Blog) }

data Pages = Pages
  { engAbout :: !(Html ())
  , japAbout :: !(Html ())
  , engCV    :: !(Html ())
  , japCV    :: !(Html ()) }

newtype Path = Path Text
  deriving stock (Generic)
  deriving anyclass (ToJSON)

pathLang :: FilePath -> Maybe Language
pathLang p = case T.take 3 . T.takeEnd 7 $ T.pack p of
  "-en" -> Just English
  "-jp" -> Just Japanese
  _     -> Nothing

pathSlug :: FilePath -> Text
pathSlug = T.dropEnd 3 . T.pack . takeBaseName

-- | For the RSS feed.
newtype ByLanguage = ByLanguage (NonEmpty Blog)

instance ToXml Blog where
  toXml (Blog l slug o@(O.OrgFile m _) _) =
    element "item" mempty
    $  element "title" mempty (text . TL.fromStrict $ fromMaybe "Untitled" title)
    <> element "link" mempty
    (text . TL.fromStrict $ "https://www.fosskers.ca/" <> langPath l <> "/blog/" <> slug)
    <> element "pubDate" mempty (text . TL.pack . dtt . dtd $ fromMaybe defDay date)
    <> element "description" mempty (text . TL.fromStrict $ fromMaybe "No description" title)
    where
      title = M.lookup "TITLE" m
      date = orgDate o

      defDay :: Day
      defDay = fromGregorian 2017 1 1

orgDate :: O.OrgFile -> Maybe Day
orgDate =
  M.lookup "DATE" . O.orgMeta >=> parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

dtd :: Day -> Date
dtd = dateFromTAIEpoch . toModifiedJulianDay

-- TODO Fri 06 Mar 2020 04:30:12 PM PST
-- It would be nice to know /why/ this is the acceptable format.
-- | Format a `Date` in a way acceptable to RSS feeds.
dtt :: Date -> String
dtt d@(Date ye mo da) = printf "%s, %d %s %d 00:00:00 GMT" wd da mo' ye
  where
    wd = take 3 . show $ getWeekDay d
    mo' = take 3 $ show mo

instance ToXml ByLanguage where
  toXml (ByLanguage bs) =
    element "rss" (HM.singleton "version" "2.0")
    $ element "channel" mempty (info <> foldMap toXml bs)
    where
      info :: [Node]
      info = element "title" mempty (text "Fosskers.ca Blog")
             <> element "link" mempty (text "https://www.fosskers.ca")
             <> element "description" mempty
             (text "Articles on Haskell, Functional Programming, and Japanese")
