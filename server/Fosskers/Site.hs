module Fosskers.Site ( index ) where

import BasePrelude hiding (index)
import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)

---

{- WHAT DO I WANT?

Topbar with links.
Landing page is blog.
Don't bother with Tools for now.

-}

index :: Html ()
index = html_ $ head_ h *> body_ (topbar *> div_ "Real content here.")
  where
    h :: Html ()
    h = do
      title_ "fosskers.ca"
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
      script_ [ src_ "assets/jquery.slim.min.js" ] ("" :: Text)
      script_ [ src_ "assets/bootstrap.min.js" ] ("" :: Text)
      link_ [ rel_ "stylesheet"
            , href_ "assets/bootstrap.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fontawesome.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fa-brands.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fa-solid.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fosskers.css" ]

topbar :: Html ()
topbar = nav_ [ classes_ [ "navbar", "navbar-expand-lg", "navbar-dark", "bg-dark" ] ] $ do
  a_ [ class_ "navbar-brand", href_ "#" ] "Fosskers"
  navButton
  div_ [ classes_ [ "collapse", "navbar-collapse" ], id_ navId ] $
    ul_ [ class_ "navbar-nav" ] $ do
      item "About" "#"
      item "Blog" "#"
      dropdown "Projects" [("Aura", "#"), ("Bag of Holding", "#"), ("MapAlgebra", "#")]
      dropdown "Tools" [("Kanji Analysis", "#")]
      item "CV" "/assets/cv.html"
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

    item :: Text -> Text -> Html ()
    item label url = li_ [ class_ "nav-item" ] $
      a_ [ class_ "nav-link", href_ url ] $ toHtml label

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
    div_ [ class_ "dropdown-menu"
         , makeAttribute "aria-labelledby" did ]
      $ traverse_ link links
  where
    did :: Text
    did = "navbarDropdown" <> label

    link :: (Text, Text) -> Html ()
    link (l, url) = a_ [ class_ "dropdown-item", href_ url ] $ toHtml l
