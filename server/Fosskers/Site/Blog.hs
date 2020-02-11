module Fosskers.Site.Blog
  ( blog
  , newest
  ) where

import Fosskers.Common (Language(..))
import Lucid
import RIO

---

newest :: Language -> Html ()
newest _ = "This is the most recent post."

blog :: Language -> Text -> Html ()
blog _ _ = "This is a selected blog."
