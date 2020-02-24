module Fosskers.Site.Blog
  ( choose
  , newest
  , blog
  ) where

import qualified Data.Map.NonEmpty as NEM
import qualified Data.Org as O
import           Data.Time.Calendar (showGregorian)
import           Fosskers.Common
import           Fosskers.Site.Bootstrap
import           Lucid
import           RIO
import qualified RIO.NonEmpty as NEL

---

newest :: Blogs -> Language -> Html ()
newest bs l = case l of
  English  -> blogHtml . NEL.head $ engSorted bs
  Japanese -> blogHtml . NEL.head $ japSorted bs

choose :: Blogs -> Language -> Text -> Html ()
choose bs l t = case l of
  English  -> fromMaybe "Not found!" $ blogHtml <$> NEM.lookup t (engPosts $ bs)
  Japanese -> fromMaybe "Not found!" $ blogHtml <$> NEM.lookup t (japPosts $ bs)

blog :: Blogs -> Language -> Html () -> Html ()
blog bs l content = row_ $ do
  col [] $ sidebar l ps
  col [] content
  where
    ps = case l of
      English  -> engSorted bs
      Japanese -> japSorted bs

sidebar :: Language -> NonEmpty Blog -> Html ()
sidebar l = traverse_ f
  where
    f :: Blog -> Html ()
    f b = div_ $ do
      a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
        $ maybe "Bug: No Title" toHtml $ O.metaTitle $ O.orgMeta $ blogRaw b
      i_ $ maybe "Bug: No Date" (toHtml . showGregorian) $ O.metaDate $ O.orgMeta $ blogRaw b
