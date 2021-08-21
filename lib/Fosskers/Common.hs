{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Fosskers.Common
  ( -- * Static Pages
    Pages(..)
  , CirVit(..)
    -- * Blog Posts
  , Blog(..)
  , BlogCategory(..)
  , catFromText, catIcon
  , BlogsByDate(..)
  , Blogs(..)
  , Title(..)
  , Language(..)
  , langPath
  , Path(..)
  , pathLang
  , pathSlug
  , ByLanguage(..)
  , orgDate
    -- * Utils
  , hush, note, noteT, (??)
  ) where

import           BasePrelude
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Aeson (ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Hourglass (getWeekDay)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Org as O
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar (Day(..))
import           Data.Time.Format
import           Lucid
import           System.FilePath.Posix (takeBaseName)
import           Time.Compat (dateFromTAIEpoch)
import           Time.Types (Date(..))
import           Xmlbf (Node, ToXml(..), element, text)

---

newtype Title = Title Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Language = English | Japanese
  deriving stock (Eq, Ord, Show, Generic)

langPath :: Language -> Text
langPath English  = "en"
langPath Japanese = "jp"

-- | Blog content converted to HTML once upon startup, along with other
-- metadata.
data Blog = Blog
  { blogLang    :: !Language
  , blogSlug    :: !Text
  , blogDate    :: !Day
  , blogUpdated :: !(Maybe Day)
  , blogCat     :: !(Maybe BlogCat)
  , blogRaw     :: !O.OrgFile
  , blogBody    :: !(Html ())
  , blogTOC     :: !(Html ()) }

-- | The category of the blog post.
data BlogCat = Tech | Haskell | Rust | Programming | Language | Food | Games

catFromText :: Text -> Maybe BlogCat
catFromText t = case t of
  "tech"        -> Just Tech
  "haskell"     -> Just Haskell
  "rust"        -> Just Rust
  "programming" -> Just Programming
  "language"    -> Just Language
  "food"        -> Just Food
  "games"       -> Just Games
  _             -> Nothing

-- | Convert a `BlogCat` into a useable Font Awesome icon.
catIcon :: BlogCat -> Html ()
catIcon c = i_ [classes_ cs] ""
  where
    cs = case c of
      Tech        -> ["fas", "fa-cog"]
      Haskell     -> ["fas", "fa-magic"]
      Rust        -> ["fab", "fa-rust"]
      Programming -> ["fas", "fa-laptop-code"]
      Language    -> ["fas", "fa-language"]
      Food        -> ["fas", "fa-utensils"]
      Games       -> ["fas", "fa-gamepad"]

data BlogCategory = BlogCategory
  { bcCat   :: !Text
  , bcBlogs :: !(NonEmpty Blog) }

data BlogsByDate = BlogsByDate
  { bbdYear  :: !Integer
  , bbdBlogs :: !(NonEmpty Blog) }

data Blogs = Blogs
  { engByCat  :: !(NonEmpty BlogCategory)
  , japByCat  :: !(NonEmpty BlogCategory)
  , engByDate :: !(NonEmpty BlogsByDate)
  , japByDate :: !(NonEmpty BlogsByDate)
  , engPosts  :: !(Map Text Blog)
  , japPosts  :: !(Map Text Blog) }

-- | CV content converted to HTML once upon startup.
data CirVit = CirVit
  { cvTitle :: !Text
  , cvBody  :: !(Html ())
  , cvTOC   :: !(Html ()) }

data Pages = Pages
  { engAbout :: !(Html ())
  , japAbout :: !(Html ())
  , engCV    :: !CirVit
  , japCV    :: !CirVit }

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
  toXml (Blog l slug day _ _ (O.OrgFile m _) _ _) =
    element "item" mempty
    $  element "title" mempty (text . TL.fromStrict $ fromMaybe "Untitled" title)
    <> element "link" mempty
    (text . TL.fromStrict $ "https://www.fosskers.ca/" <> langPath l <> "/blog/" <> slug)
    <> element "pubDate" mempty (text . TL.pack . dtt $ dtd day)
    <> element "description" mempty (text . TL.fromStrict $ fromMaybe "No description" title)
    where
      title :: Maybe Text
      title = M.lookup "TITLE" m

orgDate :: Text -> O.OrgFile -> Maybe Day
orgDate field o =
  M.lookup field (O.orgMeta o) >>= parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

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

--------------------------------------------------------------------------------
-- Utils

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

noteT :: (Monad m) => a -> MaybeT m b -> ExceptT a m b
noteT a = ExceptT . fmap (note a) . runMaybeT

infixl 9 ??
(??) :: Applicative m => Maybe a -> e -> ExceptT e m a
(??) a e = ExceptT (pure $ note e a)
