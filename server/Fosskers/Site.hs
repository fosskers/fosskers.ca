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

data Page = Landing | CV | About | Posts | Demo | Tool | Nowhere deriving (Eq)

nowhere :: Html ()
nowhere = div_ [class_ "grid-main"] $ div_ [class_ "content"] $ do
  h1_ [classes_ ["title", "is-centered"]] "404"
  p_ [class_ "is-centered"] "Unfortunately, that page doesn't exist."
  p_ [class_ "is-centered"] "残念ながらそのページは存在しません"

site :: Language -> Page -> Maybe Text -> Html () -> Html ()
site lang page title component = do
  doctype_
  html_ $ do
    head_ h
    body_ $ div_ [class_ grid] $ do
      div_ [class_ "grid-navbar"] $ topbar lang
      component
  where
    h :: Html ()
    h = do
      title_ . toHtml $ "Colin Woodbury" <> maybe "" (" - " <>) title
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      link_ [rel_ "stylesheet", href_ "/assets/css/fontawesome.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/brands.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/solid.min.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/fosskers.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/kate.css"]
      link_ [rel_ "stylesheet", href_ "/assets/css/animations.css"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/assets/images/favicon-16x16.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/assets/images/favicon-32x32.png"]

    grid :: Text
    grid = case page of
      Landing -> "grid-open"
      About   -> "grid-full"
      Demo    -> "grid-full"
      Tool    -> "grid-full"
      Nowhere -> "grid-full"
      CV      -> "grid-with-sidebar"
      Posts   -> "grid-with-sidebar"

topbar :: Language -> Html ()
topbar lang =
  nav_ [ classes_ [ "navbar", "is-dark"]
       , role_ "navigation"
       , makeAttribute "aria-label" "main navigation" ] $ do
    div_ [ class_ "navbar-brand" ] logo
    div_ [ classes_ ["navbar-menu"]] $ do
      div_ [class_ "navbar-start"] $ theBar lang
      div_ [class_ "navbar-end"] $ icons *> langButtons
  where
    theBar :: Language -> Html ()
    theBar English  = pub
    theBar Japanese = izakaya

    logo :: Html ()
    logo = a_ [ class_ "navbar-item", href_ $ "/" <> langPath lang ]
      $ img_ [ src_ "/assets/images/aura-logo.png" ]

    pub :: Html ()
    pub = do
      item "About" "fa-info-circle" "/en/about" []
      item "Blog" "fa-book" "/en/blog" []
      dropdown "Projects" "fa-code" ["mobile-hidden"] projects
      -- dropdown "Tools" [Just ("Kanji Analysis", "#")]
      dropdown "Tools" "fa-tools" ["mobile-hidden"]
        [ Just ("Al Bhed Translator", "/en/tools/al-bhed")
        , Just ("Love Letter Tracker", "/en/tools/love-letter")
        , Just ("Twitch Player", "/en/tools/twitch") ]
      dropdown "Demos" "fa-laptop-code" ["mobile-hidden"]
        [ Just ("Game of Life", "/en/demo/game-of-life")
        , Just ("Web Effects", "/en/demo/web-effects")
        , Just ("Seed Effects", "/en/demo/seed-effects") ]
      item "CV" "fa-graduation-cap" "/en/cv" ["mobile-hidden"]

    izakaya :: Html ()
    izakaya = do
      item "自己紹介" "fa-info-circle" "/jp/about" []
      item "ブログ" "fa-book" "/jp/blog" []
      dropdown "プロジェクト" "fa-code" ["mobile-hidden"] projects
      -- dropdown "ツール" [Just ("漢字分析", "#")]
      dropdown "ツール" "fa-tools" ["mobile-hidden"]
        [ Just ("アルベド翻訳", "/jp/tools/al-bhed")
        , Just ("Love Letter Tracker", "/jp/tools/love-letter")
        , Just ("Twitch Player", "/jp/tools/twitch") ]
      dropdown "デモ" "fa-laptop-code" ["mobile-hidden"]
        [ Just ("Game of Life", "/jp/demo/game-of-life")
        , Just ("ウェブ作用", "/jp/demo/web-effects")
        , Just ("Seed作用", "/jp/demo/seed-effects") ]
      item "履歴書" "fa-graduation-cap" "/jp/cv" ["mobile-hidden"]

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
      , Just ("Scala Benchmarks", "https://github.com/fosskers/scala-benchmarks")
      , Nothing
      , Just ("My Dotfiles", "https://github.com/fosskers/dotfiles")]

    -- | Highlight the navbar links according to the page we're currently on.
    item :: Html () -> Text -> Text -> [Text] -> Html ()
    item label ic url cs = a_ [ href_ url, classes_ $ "navbar-item" : cs ] $
      span_ [ class_ "icon-text"] $ do
        span_ [ class_ "icon" ] $ i_ [ classes_ ["fas", ic] ] ""
        span_ [ class_ "mobile-hidden" ] label

    icon :: Text -> [Text] -> Html ()
    icon url cs = a_ [ href_ url, classes_ ("navbar-item" : cs) ] ""

    icons :: Html ()
    icons = do
      icon "https://github.com/fosskers" [ "fab", "fa-github" ]
      a_ [ rel_ "me", href_ "https://m.fosskers.ca/@fosskers", classes_ ["navbar-item", "fab", "fa-mastodon"] ] ""
      icon "mailto:colin@fosskers.ca" [ "fas", "fa-envelope", "mobile-hidden" ]
      case lang of
        English  -> icon "/en/rss" ["fas", "fa-rss"]
        Japanese -> icon "/jp/rss" [ "fas", "fa-rss" ]

    langButtons :: Html ()
    langButtons = do
      a_ [href_ "/en", classes_ $ "navbar-item" : "mobile-hidden" : eBtn] "English"
      a_ [href_ "/jp", classes_ $ "navbar-item" : "mobile-hidden" : jBtn] "日本語"

    eBtn :: [Text]
    eBtn = case lang of
      English  -> ["is-underlined"]
      Japanese -> []

    jBtn :: [Text]
    jBtn = case lang of
      English  -> []
      Japanese -> ["is-underlined"]

-- | Construct a navbar dropdown.
dropdown :: Html () -> Text -> [Text] -> [Maybe (Html (), Text)] -> Html ()
dropdown label ic classes links =
  div_ [classes_ $ ["navbar-item", "has-dropdown", "is-hoverable"] <> classes] $ do
    a_ [class_ "navbar-link"] $
      span_ [class_ "icon-text"] $ do
        span_ [ class_ "icon" ] $ i_ [ classes_ ["fas", ic] ] ""
        span_ [ class_ "mobile-hidden" ] label
    div_ [class_ "navbar-dropdown"] $
      traverse_ link links
  where
    link :: Maybe (Html (), Text) -> Html ()
    link Nothing         = hr_ [class_ "navbar-divider"]
    link (Just (l, url)) = a_ [ class_ "navbar-item", href_ url ] l
