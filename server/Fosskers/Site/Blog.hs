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
blog bs l content = do
  div_ [class_ "grid-sidebar-right"]$ case content of
    Nothing -> ""
    Just b  -> indexBar l b
  div_ [class_ "grid-main"] $ case content of
    Nothing -> nf
    Just b  -> do
      let !m = O.orgMeta $ blogRaw b
      h1_ [classes_ ["title", "is-2", "is-centered"]] . maybe "Bug: Title Missing" toHtml $ M.lookup "TITLE" m
      div_ [classes_ ["subtitle", "is-6", "is-centered"]]. maybe "" (i_ [class_ "text-muted"] . toHtml @String) $ do
        author <- M.lookup "AUTHOR" m
        date <- M.lookup "DATE" m
        let updated = M.lookup "UPDATED" m
        Just $ printf pat author date <> maybe "" (printf upat) updated
      div_ [class_ "content"] $ blogBody b
  div_ [class_ "grid-sidebar-left"] $ articleBar l ps
  where
    (ps, nf, pat, upat) = case l of
      English  -> (engByCat bs, "Post not found!", "By %s on %s", ", updated %s")
      Japanese -> (japByCat bs, "見つかりません！", "%s・%s初出版", "・%s更新")

articleBar :: Language -> NonEmpty BlogCategory -> Html ()
articleBar l bcs = do
  p_ [classes_ ["title", "is-4", "is-centered"]] label
  div_ [class_ "left-padding"] $ do
    aside_ [class_ "menu"] $ traverse_ g bcs
  where
    g :: BlogCategory -> Html ()
    g bc = do
      p_ [class_ "menu-label"] . toHtml $ bcCat bc
      ul_ [class_ "menu-list"] $ traverse_ (li_ . f) $ bcBlogs bc

    f :: Blog -> Html ()
    f b = a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
      $ maybe "Bug: No Title" (h6_ . toHtml) $ M.lookup "TITLE" $ O.orgMeta $ blogRaw b

    label = case l of
      English  -> "Blog Archive"
      Japanese -> "ポスト一覧"

indexBar :: Language -> Blog -> Html ()
indexBar l b = do
  p_ [classes_ ["title", "is-4", "is-centered"]] label
  div_ [class_ "content"] $ blogTOC b
  where
    label = case l of
      English  -> "Table of Contents"
      Japanese -> "目次"
