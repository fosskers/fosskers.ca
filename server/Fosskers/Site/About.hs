module Fosskers.Site.About ( about ) where

import Fosskers.Common (Language(..), Pages(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)
import RIO

---

about :: Pages -> Language -> Html ()
about ps lang = do
  row_ $ div_ [classes_ ["col-md-6", "offset-md-3"]] content
  row_ $ col_ $ do -- [classes_ ["col-xs-12", "col-md-5", "offset-md-2"]] $ do
    img_ [src_ "/assets/jack.jpg", classes_ ["rounded", "mx-auto", "d-block"]]
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
