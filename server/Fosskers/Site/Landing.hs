module Fosskers.Site.Landing ( landing ) where

import BasePrelude
import Fosskers.Common
import Lucid

---

-- TODO Get total stars and downloads

landing :: Language -> Html ()
landing _ = div_ [classes_ ["grid-main", "landing"]] $ do
  span_ [classes_ ["title", "is-1"]] "Colin Woodbury"
  span_ [classes_ ["subtitle", "is-3"]] "Full-stack Software Developer"
  div_ [class_ "cards"] $ do
    div_ [class_ "card"] $ do
      div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "Rust Crates"
      div_ [class_ "card-content"] "TODO"
      div_ [class_ "card-footer"] $ do
        a_ [class_ "card-footer-item", href_ "https://crates.io/users/fosskers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-box-open"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers?tab=repositories&q=&type=source&language=rust&sort=stargazers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
    div_ [class_ "card"] $ do
      div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "The Aura Package Manager"
      div_ [class_ "card-content"] "TODO"
      div_ [class_ "card-footer"] $ do
        a_ [class_ "card-footer-item", href_ "https://fosskers.github.io/aura/"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-link"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers/aura/discussions"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-comments"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers/aura"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
    div_ [class_ "card"] $ do
      div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "Haskell Libraries"
      div_ [class_ "card-content"] "TODO"
      div_ [class_ "card-footer"] $ do
        a_ [class_ "card-footer-item", href_ "http://hackage.haskell.org/user/fosskers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-box-open"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers?tab=repositories&q=&type=source&language=haskell&sort=stargazers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
