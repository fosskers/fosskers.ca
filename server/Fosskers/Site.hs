module Fosskers.Site
  ( Page(..)
  , site
  , nowhere
  ) where

import BasePrelude
import Data.Text (Text)
import Fosskers.Common
import Lucid
import Lucid.Base (makeAttribute)

---

data Page = CV | About | Posts | Demo | Tool | Nowhere deriving (Eq)

nowhere :: Html ()
nowhere = do
  h1_ [class_ "title"] "404"
  div_ [class_ "title"] "Unfortunately, that page doesn't exist."
  div_ [class_ "title"] "残念ながらそのページは存在しません"

site :: Language -> Html () -> Html ()
site lang component = do
  doctype_
  html_ $ do
    head_ h
    body_ $ div_ [class_ "grid-container"] $ do
      div_ [class_ "grid-navbar"] $ topbar lang
      component
  where
    h :: Html ()
    h = do
      title_ "Colin Woodbury"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      link_ [rel_ "stylesheet", href_ "/assets/css/fontawesome.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/brands.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/solid.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/fosskers.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/kate.css"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/assets/images/favicon-16x16.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/assets/images/favicon-32x32.png"]

-- TODO Figure out burger menu interaction on mobile.
topbar :: Language -> Html ()
topbar lang =
  nav_ [ classes_ [ "navbar", "is-dark"]
       , role_ "navigation"
       , makeAttribute "aria-label" "main navigation" ] $ do
    div_ [ class_ "navbar-brand" ] $ logo *> burger
    -- div_ [ class_ "navbar-menu" ] $ do
    div_ [ classes_ ["navbar-menu"]] $ do
      div_ [class_ "navbar-start"] $ theBar lang
      div_ [class_ "navbar-end"] langButtons
  where
    theBar :: Language -> Html ()
    theBar English  = pub
    theBar Japanese = izakaya

    logo :: Html ()
    logo = a_ [ class_ "navbar-item", href_ $ "/" <> langPath lang ]
      $ img_ [ src_ "/assets/images/fosskers-icon.png", width_ "30", height_ "30" ]

    burger :: Html ()
    burger = a_ [ role_ "button"
                , class_ "navbar-burger"
                , makeAttribute "aria-label" "menu"
                , makeAttribute "aria-expanded" "false" ] $ do
        span_ [makeAttribute  "aria-hidden" "true"] ""
        span_ [makeAttribute  "aria-hidden" "true"] ""
        span_ [makeAttribute  "aria-hidden" "true"] ""

    pub :: Html ()
    pub = do
      item "About" "/en/about" []
      item "Blog" "/en/blog" []
      dropdown "Projects" projects
      -- dropdown "Tools" [Just ("Kanji Analysis", "#")]
      dropdown "Tools"
        [ Just ("Al Bhed Translator", "/en/tools/al-bhed")
        , Just ("Love Letter Tracker", "/en/tools/love-letter") ]
      dropdown "Demos" [Just ("Game of Life", "/en/demo/game-of-life")]
      item "CV" "/en/cv" []
      item "Freelance" "https://www.upwork.com/o/profiles/users/~01b5f223de8f22da34/" []
      icon "https://github.com/fosskers" ["fab", "fa-github"]
      icon "mailto:colin@fosskers.ca" ["fas", "fa-envelope"]
      icon "/en/rss" ["fas", "fa-rss"]
      icon "https://www.buymeacoffee.com/fosskers" ["fas", "fa-mug-hot"]

    izakaya :: Html ()
    izakaya = do
      item "自己紹介" "/jp/about" []
      item "ブログ" "/jp/blog" []
      dropdown "プロジェクト" projects
      -- dropdown "ツール" [Just ("漢字分析", "#")]
      dropdown "ツール" [ Just ("アルベド翻訳", "/jp/tools/al-bhed")
                       , Just ("Love Letter Tracker", "/jp/tools/love-letter") ]
      dropdown "デモ" [Just ("Game of Life", "/jp/demo/game-of-life")]
      item "履歴書" "/jp/cv" []
      item "受託開発" "https://www.upwork.com/o/profiles/users/~01b5f223de8f22da34/" []
      icon "https://github.com/fosskers" [ "fab", "fa-github" ]
      icon "mailto:colin@fosskers.ca" [ "fas", "fa-envelope" ]
      icon "/jp/rss" [ "fas", "fa-rss" ]
      icon "https://www.buymeacoffee.com/fosskers" ["fas", "fa-mug-hot"]

    projects :: [Maybe (Html (), Text)]
    projects =
      [ Just ("Aura", "https://github.com/fosskers/aura")
      , Just ("Credit", "https://github.com/fosskers/credit")
      , Just ("MapAlgebra", "https://github.com/fosskers/mapalgebra")
      , Nothing
      , Just ("Haskell Libraries", "http://hackage.haskell.org/user/fosskers")
      , Just ("Rust Crates", "https://crates.io/users/fosskers")
      , Just ("Go Libraries", "https://pkg.go.dev/search?q=fosskers")
      , Nothing
      , Just ("ScalaZ and Cats", "https://github.com/fosskers/scalaz-and-cats")
      , Just ("Scala Benchmarks", "https://github.com/fosskers/scala-benchmarks") ]

    -- | Highlight the navbar links according to the page we're currently on.
    item :: Html () -> Text -> [Text] -> Html ()
    item label url cs = a_ [ href_ url, classes_ $ "navbar-item" : cs ] label

    icon :: Text -> [Text] -> Html ()
    icon url cs = a_ [ href_ url, classes_ ("navbar-item" : cs) ] ""

    langButtons :: Html ()
    langButtons = do
      a_ [href_ "/en", classes_ $ "navbar-item" : eBtn] "English"
      a_ [href_ "/jp", classes_ $ "navbar-item" : jBtn] "日本語"

    eBtn :: [Text]
    eBtn = case lang of
      English  -> ["is-underlined"]
      Japanese -> []

    jBtn :: [Text]
    jBtn = case lang of
      English  -> []
      Japanese -> ["is-underlined"]

-- | Construct a Bootstrap navbar dropdown.
dropdown :: Text -> [Maybe (Html (), Text)] -> Html ()
dropdown label links =
  div_ [classes_ ["navbar-item", "has-dropdown", "is-hoverable"]] $ do
    a_ [class_ "navbar-link"] $ toHtml label
    div_ [class_ "navbar-dropdown"] $
      traverse_ link links
  where
    link :: Maybe (Html (), Text) -> Html ()
    link Nothing         = hr_ [class_ "navbar-divider"]
    link (Just (l, url)) = a_ [ class_ "navbar-item", href_ url ] l
