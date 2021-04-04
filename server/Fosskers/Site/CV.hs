module Fosskers.Site.CV ( cv ) where

import Fosskers.Common (Language(..), Pages(..))
import Lucid hiding (col_)

---

cv :: Pages -> Language -> Html ()
cv ps lang = content
  where
    content :: Html ()
    content = case lang of
      English  -> engCV ps
      Japanese -> japCV ps
