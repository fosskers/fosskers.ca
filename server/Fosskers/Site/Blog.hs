module Fosskers.Site.Blog
  ( blog
  , newest
  ) where

import           Fosskers.Common (Blog(..), Language(..))
import           Lucid
import           RIO
import qualified RIO.NonEmpty as NEL

---

newest :: NonEmpty Blog -> NonEmpty Blog -> Language -> Html ()
newest ens jps l = case l of
  English  -> blogHtml $ NEL.head ens
  Japanese -> blogHtml $ NEL.head jps

blog :: NonEmpty Blog -> NonEmpty Blog -> Language -> Text -> Html ()
blog ens jps l _ = case l of
  English  -> blogHtml $ NEL.head ens
  Japanese -> blogHtml $ NEL.head jps
