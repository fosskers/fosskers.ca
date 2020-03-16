module Fosskers.Site.Bootstrap where

import Lucid

---

fluid :: [Attribute] -> Html () -> Html ()
fluid as hs = div_ (class_ "container-fluid" : as) hs

row :: [Attribute] -> Html () -> Html ()
row as hs = div_ (class_ "row" : as) hs

-- | A simple shorthand for Bootstrap grid rows.
row_ :: Html () -> Html ()
row_ = row []

col :: [Attribute] -> Html () -> Html ()
col as hs = div_ (class_ "col" : as) hs

-- | A simple shorthand for Bootstrap grid columns.
col_ :: Html () -> Html ()
col_ = col []
