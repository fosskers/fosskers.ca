module Fosskers.Site.Blog
  ( choose
  , newest
  , blog
  ) where

import qualified Data.Map.NonEmpty as NEM
import qualified Data.Org as O
import           Data.Time.Calendar (toGregorian)
import           Fosskers.Common
import           Fosskers.Site.Bootstrap
import           Lens.Micro ((^?), _1, _Just)
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
  div_ [classes_ ["col-xs-12", "col-md-3"]] $ sidebar l ps
  div_ [classes_ ["col-xs-12", "col-md-6"]] content
  div_ [class_ "col-md-3"] ""
  where
    ps = case l of
      English  -> engSorted bs
      Japanese -> japSorted bs

sidebar :: Language -> NonEmpty Blog -> Html ()
sidebar l bs = traverse_ g $ NEL.groupWith1 year bs
  where
    year :: Blog -> Maybe Integer
    year b = b ^? to (O.metaDate . O.orgMeta . blogRaw) . _Just . to toGregorian . _1

    g :: NonEmpty Blog -> Html ()
    g b = do
      h5_ . b_ . maybe "Unknown" (toHtml . show) . year $ NEL.head b
      ul_ $ traverse_ (li_ . f) b

    f :: Blog -> Html ()
    f b = div_ $ do
      a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
        $ maybe "Bug: No Title" (h6_ . toHtml) $ O.metaTitle $ O.orgMeta $ blogRaw b
