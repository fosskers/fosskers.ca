module Fosskers.Site.CV ( cv ) where

import BasePrelude
import Fosskers.Common (Language(..), Pages(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)

---

cv :: Pages -> Language -> Html ()
cv ps lang = row_ $ div_ [classes_ ["col-md-6", "offset-md-3"]] content
  where
    content :: Html ()
    content = case lang of
      English  -> engCV ps
      Japanese -> japCV ps
