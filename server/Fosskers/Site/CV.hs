module Fosskers.Site.CV ( cv ) where

import BasePrelude
import Fosskers.Common (CirVit(..), Language(..), Pages(..))
import Lucid hiding (col_)

---

cv :: Pages -> Language -> Html ()
cv ps lang = do
  div_ [classes_ ["grid-sidebar-left"]] $ div_ [class_ "content"] $ cvTOC c
  div_ [classes_ ["grid-main"]] $ do
    h1_ [classes_ ["title", "is-2", "is-centered"]] $ toHtml $ cvTitle c
    div_ [class_ "content"] $ do
      figure_ $ img_ [src_ "/assets/images/colin-profile.jpg"]
      cvBody c
  where
    c :: CirVit
    c = case lang of
      English  -> engCV ps
      Japanese -> japCV ps
