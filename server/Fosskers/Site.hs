module Fosskers.Site where

import BasePrelude
import Data.Text (Text)
import Lucid

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
      -- meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
      -- script_ [ src_ "assets/jquery.slim.min.js" ] ("" :: Text)
      -- script_ [ src_ "assets/bootstrap.min.js" ] ("" :: Text)
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/bootstrap.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fontawesome.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fa-brands.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fa-solid.min.css" ]
      -- link_ [ rel_ "stylesheet"
      --       , href_ "assets/fosskers.css" ]

topbar :: Html ()
topbar = div_ $ do
  "About"
  "Tools"
  "Blog"
  "CV"
  "RSS"
  "Email / Social Links"

blog :: Text -> Html ()
blog = undefined
