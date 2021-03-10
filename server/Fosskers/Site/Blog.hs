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
import           Lucid.Base (makeAttribute)

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
  div_ [style_ "padding-top: 1.0%", classes_ ["col-xs-12", "col-md-3"]] $ case content of
    Nothing -> ""
    Just b  -> indexBar l b
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
      div_ [style_ "padding-top: 1.0%"] $ blogBody b
  div_ [style_ "padding-top: 1.0%", classes_ ["col-xs-12", "col-md-3"]] $ articleBar l ps
  where
    (ps, nf, pat, upat) = case l of
      English  -> (engByCat bs, "Post not found!", "By %s on %s", ", updated %s")
      Japanese -> (japByCat bs, "見つかりません！", "%s・%s初出版", "・%s更新")

articleBar :: Language -> NonEmpty BlogCategory -> Html ()
articleBar l bcs = do
  div_ [class_ "title"] $ h3_ label
  traverse_ g bcs
  div_ [class_ "title"] coffee
  where
    g :: BlogCategory -> Html ()
    g bc = do
      h5_ . b_ . toHtml $ bcCat bc
      ul_ $ traverse_ (li_ . f) $ bcBlogs bc

    f :: Blog -> Html ()
    f b = div_
      $ a_ [href_ $ "/" <> langPath l <> "/blog/" <> blogSlug b]
      $ maybe "Bug: No Title" (h6_ . toHtml) $ M.lookup "TITLE" $ O.orgMeta $ blogRaw b

    label = case l of
      English  -> "Blog Archive"
      Japanese -> "ポスト一覧"

    coffee :: Html ()
    coffee = script_ [ type_ "text/javascript"
                     , src_ "https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js"
                     , makeAttribute "data-name" "bmc-button"
                     , makeAttribute "data-slug" "fosskers"
                     , makeAttribute "data-color" "#FFDD00"
                     , makeAttribute "data-emoji" ""
                     , makeAttribute "data-font" "Cookie"
                     , makeAttribute "data-text" "Buy me a coffee"
                     , makeAttribute "data-outline-color" "#000000"
                     , makeAttribute "data-font-color" "#000000"
                     , makeAttribute "data-coffee-color" "#ffffff"
                     ] ("" :: Text)

indexBar :: Language -> Blog -> Html ()
indexBar l b = do
  div_ [class_ "title"] $ h3_ label
  blogTOC b
  where
    label = case l of
      English  -> "Table of Contents"
      Japanese -> "目次"
