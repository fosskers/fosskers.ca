module Fosskers.Site.Landing ( landing ) where

import BasePrelude
import Fosskers.Common
import Lucid

---

landing :: Language -> Html ()
landing _ = div_ [classes_ ["grid-open-main", "landing"]] $ do
  span_ [classes_ ["title", "is-1", "my-name"]] "Colin Woodbury"
  span_ [classes_ ["subtitle", "is-3", "my-role"]] "Full-stack Software Developer"
  span_ [classes_ ["subtitle", "is-6", "my-passions"]] $ em_ "I love programming, rock climbing, languages, and playing the bass!"
  div_ [class_ "cards"] $ do
    div_ [classes_ ["card", "card-rust"]] $ do
      div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "Rust Crates"
      div_ [class_ "card-content"] $ span_ [classes_  ["title", "is-5"]] "14,000+ Downloads"
      div_ [class_ "card-footer"] $ do
        a_ [class_ "card-footer-item", href_ "https://crates.io/users/fosskers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-box-open"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers?tab=repositories&q=&type=source&language=rust&sort=stargazers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
    div_ [classes_ ["card", "card-aura"]] $ do
      div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "The Aura Package Manager"
      div_ [class_ "card-content"] $ span_ [classes_ ["title", "is-5"]] "1,000+ Stars・90,000+ Downloads"
      div_ [class_ "card-footer"] $ do
        a_ [class_ "card-footer-item", href_ "https://fosskers.github.io/aura/"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-link"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers/aura/discussions"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-comments"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers/aura"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
    div_ [classes_ ["card", "card-haskell"]] $ do
      div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "Haskell Libraries"
      div_ [class_ "card-content"] $ span_ [classes_ ["title", "is-5"]] "70,000+ Downloads"
      div_ [class_ "card-footer"] $ do
        a_ [class_ "card-footer-item", href_ "http://hackage.haskell.org/user/fosskers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-box-open"]] ""
        a_ [class_ "card-footer-item", href_ "https://github.com/fosskers?tab=repositories&q=&type=source&language=haskell&sort=stargazers"] $
          span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
  div_ [classes_ ["my-blog"]] $ do
    "Check out my "
    a_ [href_ "/en/blog"] "Blog"
    "!"
