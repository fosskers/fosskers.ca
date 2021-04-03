module Fosskers.Site.About ( about ) where

import BasePrelude
import Fosskers.Common (Language(..), Pages(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)

---

about :: Pages -> Language -> Html ()
about ps lang = do
  row_ $ div_ [classes_ ["col-md-6", "offset-md-3"]] content
  row_ $ col_ $ do
    img_ [src_ "/assets/images/jack.jpg", classes_ ["rounded", "mx-auto", "d-block"]]
    div_ [class_ "text-center"] $ i_ cat
  where
    cat :: Html ()
    cat = case lang of
      English  -> "Jack in a sunbeam."
      Japanese -> "日差しを浴びるジャック"

    content :: Html ()
    content = case lang of
      English  -> engAbout ps
      Japanese -> japAbout ps
