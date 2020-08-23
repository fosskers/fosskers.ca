module Fosskers.Site
  ( Page(..)
  , site
  , nowhere
  ) where

import BasePrelude
import Data.Text (Text)
import Fosskers.Common
import Fosskers.Site.Bootstrap
import Lucid
import Lucid.Base (makeAttribute)

---

{- WHAT DO I WANT?

Topbar with links.
Landing page is blog.
Don't bother with Tools for now.

-}

data Page = CV | About | Posts | Demo | Nowhere deriving (Eq)

nowhere :: Html ()
nowhere = do
  h1_ [class_ "title"] "404"
  div_ [class_ "title"] "Unfortunately, that page doesn't exist."
  div_ [class_ "title"] "残念ながらそのページは存在しません"

site :: Language -> Page -> Html () -> Html ()
site lang page component = html_ $ do
  head_ h
  body_ $ do
    topbar lang page
    fluid [style_ "padding-top: 1.0%;padding-bottom: 1.0%"] component
  where
    h :: Html ()
    h = do
      title_ "Colin Woodbury"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      script_ [src_ "/assets/jquery.slim.min.js"] ("" :: Text)
      script_ [src_ "/assets/bootstrap.min.js"] ("" :: Text)
      link_ [rel_ "stylesheet", href_ "/assets/bootstrap.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/fontawesome.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/fa-brands.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/fa-solid.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/fosskers.css"]
      link_ [rel_ "stylesheet", href_ "/assets/kate.css"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/assets/favicon-16x16.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/assets/favicon-32x32.png"]

topbar :: Language -> Page -> Html ()
topbar lang page = nav_ [ classes_ [ "navbar", "navbar-expand-md", "navbar-dark", "bg-dark" ] ] $ do
  a_ [ class_ "navbar-brand", href_ $ "/" <> langPath lang ]
    $ img_ [ src_ "/assets/fosskers-icon.png", width_ "30", height_ "30" ]
  navButton
  div_ [ classes_ [ "collapse", "navbar-collapse" ], id_ navId ]
    $ ul_ [ class_ "navbar-nav" ]
    $ theBar lang
  langButtons
  where
    navId :: Text
    navId = "navbarLinks"

    navButton :: Html ()
    navButton = button_
      [ class_ "navbar-toggler"
      , type_ "button"
      , makeAttribute "data-toggle" "collapse"
      , makeAttribute "data-target" $ "#" <> navId
      , makeAttribute "aria-controls" navId
      , makeAttribute "aria-expanded" "false"
      , makeAttribute "aria-label" "Toggle navigation" ]
      $ span_ [ class_ "navbar-toggler-icon" ] ""

    theBar :: Language -> Html ()
    theBar English  = pub
    theBar Japanese = izakaya

    pub :: Html ()
    pub = do
      item "About" "/en/about" $ active About
      item "Blog" "/en/blog" $ active Posts
      dropdown "Projects" projects
      -- dropdown "Tools" [Just ("Kanji Analysis", "#")]
      dropdown "Demos" [Just ("Game of Life", "/en/demo/game-of-life")]
      item "CV" "/en/cv" []
      icon "https://github.com/fosskers" ["fab", "fa-github"]
      icon "https://twitter.com/fosskers" ["fab", "fa-twitter"]
      icon "mailto:colin@fosskers.ca" ["fas", "fa-envelope"]
      icon "/en/rss" ["fas", "fa-rss"]

    izakaya :: Html ()
    izakaya = do
      item "自己紹介" "/jp/about" $ active About
      item "ブログ" "/jp/blog" $ active Posts
      dropdown "プロジェクト" projects
      -- dropdown "ツール" [Just ("漢字分析", "#")]
      dropdown "デモ" [Just ("Game of Life", "/jp/demo/game-of-life")]
      item "履歴書" "/jp/cv" []
      icon "https://github.com/fosskers" [ "fab", "fa-github" ]
      icon "https://twitter.com/fosskers" [ "fab", "fa-twitter" ]
      icon "mailto:colin@fosskers.ca" [ "fas", "fa-envelope" ]
      icon "/jp/rss" [ "fas", "fa-rss" ]

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
    active :: Page -> [Text]
    active p = bool [] ["active", "font-weight-bold"] $ p == page

    item :: Html () -> Text -> [Text] -> Html ()
    item label url cs = li_ [ classes_ $ "nav-item" : cs ]
      $ a_ [ class_ "nav-link", href_ url ] label

    icon :: Text -> [Text] -> Html ()
    icon url cs = li_ [ class_ "nav-item" ]
      $ a_ [ href_ url, classes_ ("nav-link" : cs), style_ "font-size: 1.33333em" ] ""

    langButtons :: Html ()
    langButtons = div_
      [ class_ "btn-group"
      , role_ "group"
      , makeAttribute "aria-label" "Language" ] $ do
        a_ [ classes_ [ "btn", eBtn ], role_ "button", href_ "/en" ] "English"
        a_ [ classes_ [ "btn", jBtn ], role_ "button", href_ "/jp" ] "日本語"

    eBtn :: Text
    eBtn = case lang of
      English  -> "btn-info"
      Japanese -> "btn-outline-info"

    jBtn :: Text
    jBtn = case lang of
      English  -> "btn-outline-info"
      Japanese -> "btn-info"

-- | Construct a Bootstrap navbar dropdown.
dropdown :: Text -> [Maybe (Html (), Text)] -> Html ()
dropdown label links =
  li_ [ classes_ [ "nav-item", "dropdown" ] ] $ do
    a_ [ classes_ [ "nav-link", "dropdown-toggle" ]
       , href_ "#"
       , id_ did
       , role_ "button"
       , makeAttribute "data-toggle" "dropdown"
       , makeAttribute "aria-haspopup" "true"
       , makeAttribute "aria-expanded" "false" ] $ toHtml label
    div_ [ class_ "dropdown-menu", makeAttribute "aria-labelledby" did ]
      $ traverse_ link links
  where
    did :: Text
    did = "navbarDropdown" <> label

    link :: Maybe (Html (), Text) -> Html ()
    link Nothing         = div_ [class_ "dropdown-divider"] ""
    link (Just (l, url)) = a_ [ class_ "dropdown-item", href_ url ] l
