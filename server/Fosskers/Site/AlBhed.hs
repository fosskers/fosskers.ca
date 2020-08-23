module Fosskers.Site.AlBhed ( alBhed ) where

import BasePrelude
import Fosskers.Common (Language(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)
import Lucid.Base (termRawWith)

---

alBhed :: Language -> Html ()
alBhed _ = do
  div_ [class_ "title"] $ do
    h1_ "Al Bhed Translator"
    "Al Bhed is the language of the Al Bhed people in the land of Spira, the setting for the game Final Fantasy X."
  div_ [class_ "title"] "Type in either box to see the translated result in the other."
  fluid [style_ "padding-top: 1.0%;padding-bottom: 1.0%"] $ row_ $ do
    div_ [classes_ ["col-md-3", "offset-md-3"]] $ do
      div_ [class_ "title"] $ h3_ "English"
    div_ [classes_ ["col-md-3"]] $ do
      div_ [class_ "title"] $ h3_ "Al Bhed"
