module Content ( component, Query(..) ) where

import Prelude

import About (about)
import Blog as Blog
import CSS (display, displayNone)
import Control.Error.Util (bool)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Fosskers.Common (Language)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Kanji as Kanji
import Types (Effects, Tab(..), defaultLang, defaultTab, update)

---

data Query a = LangChanged Language a | TabChanged Tab a

type State = { language :: Language, tab :: Tab }

type ChildQuery = Coproduct2 Blog.Query Kanji.Query
type ChildSlot = Either2 Unit Unit

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component = H.parentComponent { initialState: const { language: defaultLang, tab: defaultTab }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall e. State -> H.ParentHTML Query ChildQuery ChildSlot (Effects e)
render state = HH.div_
               [ HH.div (hide Blog)
                 [ HH.slot' CP.cp1 unit Blog.component state.language absurd ]
               , HH.div (hide About) [ about state.language ]
               , HH.div (hide Kanji)
                 [ HH.slot' CP.cp2 unit Kanji.component state.language absurd ]]
  where hide t = bool [ CSS.style $ display displayNone ] [] (t == state.tab)

eval :: forall e. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Effects e)
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  TabChanged t next  -> update (prop (SProxy :: SProxy "tab")) t *> pure next
