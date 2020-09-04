{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module Fosskers.Site.Blog
  ( choose
  , newest
  , blog
  ) where

import           BasePrelude
import qualified Data.Map.Strict as M
import qualified Data.Org as O
import           Data.Text (Text)
import           Fosskers.Common
import           Fosskers.Site.Bootstrap
import           Lucid hiding (for_)

---

newest :: Blogs -> Language -> Blog
newest bs l = case l of
  English  -> engNewest bs
  Japanese -> japNewest bs

choose :: Blogs -> Language -> Text -> Maybe Blog
choose bs l t = case l of
  English  -> M.lookup t (engPosts bs)
  Japanese -> M.lookup t (japPosts bs)

blog :: Blogs -> Language -> Maybe Blog -> Html ()
blog bs l content = row_ $ do
  div_ [classes_ ["col-xs-12", "col-md-3"]] $ articleBar l ps
  div_ [classes_ ["col-xs-12", "col-md-6"]] $ case content of
    Nothing -> nf
    Just b  -> do
      let !m = O.orgMeta $ blogRaw b
      h1_ [class_ "title"] . maybe "Bug: Title Missing" toHtml $ M.lookup "TITLE" m
      div_ [class_ "title"] . maybe "" (i_ [class_ "text-muted"] . toHtml @String) $ do
        author <- M.lookup "AUTHOR" m
        date <- M.lookup "DATE" m
        let updated = M.lookup "UPDATED" m
        Just $ printf pat author date <> maybe "" (printf upat) updated
      blogHtml b
  div_ [classes_ ["col-xs-12", "col-md-3"]] $ indexBar l
  where
    (ps, nf, pat, upat) = case l of
      English  -> (engByCat bs, "Post not found!", "By %s on %s", ", updated %s")
      Japanese -> (japByCat bs, "見つかりません！", "%s・%s初出版", "・%s更新")

articleBar :: Language -> NonEmpty BlogCategory -> Html ()
articleBar l bcs = do
  div_ [class_ "title"] $ h3_ "Archive"
  traverse_ g bcs
  where
    g :: BlogCategory -> Html ()
    g bc = do
      h5_ . b_ . toHtml $ bcCat bc
      ul_ $ traverse_ (li_ . f) $ bcBlogs bc

    f :: Blog -> Html ()
    f b = div_
      $ a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
      $ maybe "Bug: No Title" (h6_ . toHtml) $ M.lookup "TITLE" $ O.orgMeta $ blogRaw b

indexBar :: Language -> Html ()
indexBar l = do
  div_ [class_ "title"] $ h3_ "Contents"
