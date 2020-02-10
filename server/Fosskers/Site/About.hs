module Fosskers.Site.About ( about ) where

import Fosskers.Common (Language(..))
import Lucid

---

about :: Language -> Html ()
about _ = "Great about page!"
