module Fosskers.Site.About ( about ) where

import BasePrelude
import Fosskers.Common (Language(..), Pages(..))
import Lucid hiding (col_)

---

about :: Pages -> Language -> Html ()
about ps lang = do
  div_ [classes_ []] content
  div_ $ do
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
