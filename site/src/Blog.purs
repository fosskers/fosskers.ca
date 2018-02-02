module Blog ( component, Query(..) ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Search as Search
import Types (Language(English))

---

data Query a = LangChanged Language a | Display (Array String) a

type State = { language :: Language }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.parentComponent { initialState: const { language: English }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render state = HH.div_
  [ HH.slot SearchSlot Search.component state.language (HE.input Display) ]

eval :: forall m. Query ~> H.ParentDSL State Query Search.Query Slot Void m
eval = case _ of
  LangChanged l next -> do
    lang <- H.gets _.language
    unless (l == lang) $ H.modify (_  { language = l })
    pure next
  Display kws next -> pure next
