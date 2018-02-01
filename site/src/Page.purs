module Page where

import Data.Functor.Coproduct.Nested (Coproduct2)
import Prelude

import Data.Either.Nested (Either2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import LangToggle as LangToggle
import Search as Search
import Types (Language(..))

---

data Query a = LangChanged Language a

type State = { language :: Language }

type ChildQuery = Coproduct2 LangToggle.Query Search.Query

type ChildSlot = Either2 Unit Unit

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.parentComponent { initialState: const { language: English }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall m. State -> H.ParentHTML Query ChildQuery ChildSlot m
render state = HH.div_
  [ HH.slot' CP.cp1 unit LangToggle.component unit (\l -> Just $ LangChanged l unit)
  , HH.slot' CP.cp2 unit Search.component state.language absurd ]

eval :: forall m. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
eval = case _ of
  LangChanged l next -> H.modify (_ { language = l }) *> pure next
