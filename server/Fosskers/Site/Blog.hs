module Fosskers.Site.Blog
  ( blog
  , newest
  ) where

import           Fosskers.Common (Blog(..), Language(..))
import           Lucid
import           RIO
import qualified RIO.Map as M

---

newest :: Blog -> Blog -> Language -> Html ()
newest en jp l = case l of
  English  -> blogHtml en
  Japanese -> blogHtml jp

blog :: Map Text Blog -> Map Text Blog -> Language -> Text -> Html ()
blog ens jps l t = case l of
  English  -> fromMaybe "Not found!" (blogHtml <$> M.lookup t ens)
  Japanese -> fromMaybe "Not found!" (blogHtml <$> M.lookup t jps)
