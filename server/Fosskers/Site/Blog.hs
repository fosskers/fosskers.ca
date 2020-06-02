{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module Fosskers.Site.Blog
  ( choose
  , newest
  , blog
  ) where

import           BasePrelude
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.Org as O
import           Data.Text (Text)
import           Data.Time.Calendar (toGregorian)
import           Fosskers.Common
import           Fosskers.Site.Bootstrap
import           Lens.Micro (to, (^?), _1, _Just)
import           Lucid hiding (for_)

---

newest :: Blogs -> Language -> Blog
newest bs l = case l of
  English  -> NEL.head $ engSorted bs
  Japanese -> NEL.head $ japSorted bs

choose :: Blogs -> Language -> Text -> Maybe Blog
choose bs l t = case l of
  English  -> M.lookup t (engPosts bs)
  Japanese -> M.lookup t (japPosts bs)

blog :: Blogs -> Language -> Maybe Blog -> Html ()
blog bs l content = row_ $ do
  div_ [classes_ ["col-xs-12", "col-md-3"]] $ sidebar l ps
  div_ [classes_ ["col-xs-12", "col-md-6"]] $ case content of
    Nothing -> nf
    Just b  -> do
      let !m = O.orgMeta $ blogRaw b
      h1_ [class_ "title"] . maybe "Title Missing" toHtml $ M.lookup "TITLE" m
      div_ [class_ "title"] . maybe "" (i_ . toHtml @String) $ do
        author <- M.lookup "AUTHOR" m
        date <- M.lookup "DATE" m
        let updated = M.lookup "UPDATED" m
        Just $ printf pat author date <> maybe "" (printf upat) updated
      blogHtml b
  div_ [class_ "col-md-3"] ""
  where
    (ps, nf, pat, upat) = case l of
      English  -> (engSorted bs, "Post not found!", "By %s on %s", ", updated %s")
      Japanese -> (japSorted bs, "見つかりません！", "%s・%s初出版", "・%s更新")

sidebar :: Language -> NonEmpty Blog -> Html ()
sidebar l bs = traverse_ g $ NEL.groupWith1 year bs
  where
    year :: Blog -> Maybe Integer
    year b = b ^? to (orgDate . blogRaw) . _Just . to toGregorian . _1

    g :: NonEmpty Blog -> Html ()
    g b = do
      h5_ . b_ . maybe "Unknown" (toHtml . show) . year $ NEL.head b
      ul_ $ traverse_ (li_ . f) b

    f :: Blog -> Html ()
    f b = div_
      $ a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
      $ maybe "Bug: No Title" (h6_ . toHtml) $ M.lookup "TITLE" $ O.orgMeta $ blogRaw b
