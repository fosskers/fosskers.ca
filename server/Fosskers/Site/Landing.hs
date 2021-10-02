module Fosskers.Site.Landing ( landing ) where

import BasePrelude
import Fosskers.Common
import Lucid

---

landing :: Language -> Html ()
landing lang = div_ [classes_ ["grid-open-main", "landing"]] $ do
  animName
  animRole
  animPassions
  cards
  animBlog
  where
    animName :: Html ()
    animName = span_ [classes_ ["title", "is-1", "my-name" ]] name

    name :: Html ()
    name = case lang of
      English  -> "Colin Woodbury"
      Japanese -> "ウッドブリ・コリン"

    animRole :: Html ()
    animRole = span_ [classes_ ["subtitle", "is-3", "my-role"]] role

    role :: Html ()
    role = case lang of
      English  -> "Full-stack Software Developer"
      Japanese -> "フルスタック開発者"

    animPassions :: Html ()
    animPassions = p_ [classes_ ["my-passions"]] passions

    passions :: Html ()
    passions = em_ $ case lang of
      English  -> "I love programming, rock climbing, languages, and playing the bass!"
      Japanese -> "プログラミング・クライミング・言語学修・ベース弾奏を趣味としています"

    animBlog :: Html ()
    animBlog = p_ [classes_ ["my-blog"]] blog

    blog :: Html ()
    blog = case lang of
      English  -> "Check out my " *> a_ [href_ "/en/blog"] "Blog" *> "!"
      Japanese -> "是非" *> a_ [href_ "/jp/blog"] "ブログ" *> "をご覧に！"

cards :: Html ()
cards = div_ [class_ "cards"] $ do
  div_ [classes_ ["card", "card-rust"]] $ do
    div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "Rust Crates"
    div_ [class_ "card-content"] $ span_ [classes_  ["title", "is-5"]] "67,000+ Downloads"
    div_ [class_ "card-footer"] $ do
      a_ [class_ "card-footer-item", href_ "https://crates.io/users/fosskers"] $
        span_ [class_ "icon"] $ i_ [classes_ ["fas", "fa-box-open"]] ""
      a_ [class_ "card-footer-item", href_ "https://github.com/fosskers?tab=repositories&q=&type=source&language=rust&sort=stargazers"] $
        span_ [class_ "icon"] $ i_ [classes_ ["fab", "fa-github"]] ""
  div_ [classes_ ["card", "card-aura"]] $ do
    div_ [class_ "card-header"] $ p_ [class_ "card-header-title"] "The Aura Package Manager"
    div_ [class_ "card-content"] $ span_ [classes_ ["title", "is-5"]] "1,000+ Stars・100,000+ Downloads"
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
