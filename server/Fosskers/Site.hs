module Fosskers.Site
  ( site
  , index
  ) where

import BasePrelude hiding (index)
import Data.Text (Text)
import Fosskers.Common (Language(..))
import Lucid
import Lucid.Base (makeAttribute)

---

{- WHAT DO I WANT?

Topbar with links.
Landing page is blog.
Don't bother with Tools for now.

-}

index :: Language -> Html ()
index lang = site lang "Real content here!"

site :: Language -> Html () -> Html ()
site lang component = html_ $ head_ h *> body_ (topbar lang *> component)
  where
    h :: Html ()
    h = do
      title_ "Colin Woodbury"
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
      script_ [ src_ "/assets/jquery.slim.min.js" ] ("" :: Text)
      script_ [ src_ "/assets/bootstrap.min.js" ] ("" :: Text)
      link_ [ rel_ "stylesheet", href_ "/assets/bootstrap.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fontawesome.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fa-brands.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fa-solid.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fosskers.css" ]

topbar :: Language -> Html ()
topbar lang = nav_ [ classes_ [ "navbar", "navbar-expand-lg", "navbar-dark", "bg-dark" ] ] $ do
  a_ [ class_ "navbar-brand", href_ "#" ]
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
      item "About" "#"
      item "Blog" "/en/blog"
      dropdown "Projects" [("Aura", "#"), ("Bag of Holding", "#"), ("MapAlgebra", "#")]
      dropdown "Tools" [("Kanji Analysis", "#")]
      item "CV" "/assets/cv.html"

    izakaya :: Html ()
    izakaya = do
      item "自己紹介" "#"
      item "ブログ" "/jp/blog"
      dropdown "プロジェクト" [("Aura", "#"), ("Bag of Holding", "#"), ("MapAlgebra", "#")]
      dropdown "ツール" [("漢字分析", "#")]
      item "履歴書" "/assets/cv-jp.html"

    item :: Text -> Text -> Html ()
    item label url = li_ [ class_ "nav-item" ]
      $ a_ [ class_ "nav-link", href_ url ]
      $ toHtml label

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
dropdown :: Text -> [(Text, Text)] -> Html ()
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

    link :: (Text, Text) -> Html ()
    link (l, url) = a_ [ class_ "dropdown-item", href_ url ] $ toHtml l
