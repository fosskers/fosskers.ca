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
import           Data.Time (Day)
import           Data.Time.Calendar (diffDays)
import           Fosskers.Common
import           Lucid hiding (for_)

---

newest :: Blogs -> Language -> Blog
newest bs l = NEL.head . bbdBlogs . NEL.head $ case l of
  English  -> engByDate bs
  Japanese -> japByDate bs

choose :: Blogs -> Language -> Text -> Maybe Blog
choose bs l t = case l of
  English  -> M.lookup t (engPosts bs)
  Japanese -> M.lookup t (japPosts bs)

blog :: Day -> Blogs -> Language -> Maybe Blog -> Html ()
blog today bs l content = do
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
  div_ [class_ "grid-sidebar-left"] $ articleBar' today l ps
  where
    (ps, nf, pat, upat) = case l of
      English  -> (engByDate bs, "Post not found!", "By %s on %s", ", updated %s")
      Japanese -> (japByDate bs, "見つかりません！", "%s・%s初出版", "・%s更新")

-- TODO Reinstate this for the categorical view of the blog entries.
-- articleBar :: Language -> NonEmpty BlogCategory -> Html ()
-- articleBar l bcs = do
--   p_ [classes_ ["title", "is-4", "is-centered"]] label
--   div_ [class_ "left-padding"] $ do
--     aside_ [class_ "menu"] $ traverse_ g bcs
--   where
--     g :: BlogCategory -> Html ()
--     g bc = do
--       p_ [class_ "menu-label"] . toHtml $ bcCat bc
--       ul_ [class_ "menu-list"] $ traverse_ (li_ . f) $ bcBlogs bc

--     f :: Blog -> Html ()
--     f b = a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
--       $ maybe "Bug: No Title" (h6_ . toHtml) $ M.lookup "TITLE" $ O.orgMeta $ blogRaw b

--     label = case l of
--       English  -> "Blog Archive"
--       Japanese -> "ポスト一覧"

articleBar' :: Day -> Language -> NonEmpty BlogsByDate -> Html ()
articleBar' today l bbds = do
  p_ [classes_ ["title", "is-4", "is-centered"]] label
  div_ [class_ "left-padding"] $ do
    aside_ [class_ "menu"] $ traverse_ g bbds
  where
    g :: BlogsByDate -> Html ()
    g b = do
      p_ [class_ "menu-label"] . toHtml . show $ bbdYear b
      ul_ [class_ "menu-list"] $ traverse_ (li_ . f) $ bbdBlogs b

    f :: Blog -> Html ()
    f b = a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b] $ do
      div_ [class_ "blog-title"] $ do
        span_ . maybe (i_ [classes_ ["fas", "fa-question"]] "") catIcon $ blogCat b
        span_ . maybe "Bug: No Title" toHtml . M.lookup "TITLE" . O.orgMeta $ blogRaw b
        let days = diffDays today <$> blogUpdated b
            updated = maybe False (< 60) days
            new = diffDays today (blogDate b) <= 35
        when new $ span_ [classes_ ["tag", "is-info", "is-rounded"]] "New!"
        when updated $ span_ [classes_ ["tag", "is-warning", "is-rounded"]] "Updated!"

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
