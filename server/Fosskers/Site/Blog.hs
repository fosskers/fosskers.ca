module Fosskers.Site.Blog
  ( blog
  , newest
  ) where

import Data.Text (Text)
import Fosskers.Common (Language(..))
import Lucid

---

newest :: Language -> Html ()
newest _ = "This is the most recent post."

blog :: Language -> Text -> Html ()
blog _ _ = "This is a selected blog."
