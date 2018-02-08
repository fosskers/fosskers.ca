module Page where

import Prelude

import Bar as Bar
import Content as Content
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Types (Language(..), Tab, Effects)

---

data Query a = LangChanged Language a | TabChanged Tab a

type ChildQuery = Coproduct2 Bar.Query Content.Query

type ChildSlot = Either2 Unit Unit

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component = H.parentComponent { initialState: const { language: English }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall s e. s -> H.ParentHTML Query ChildQuery ChildSlot (Effects e)
render _ = HH.div_
           [ HH.slot' CP.cp1 unit Bar.component unit (Just <<< f)
           , HH.slot' CP.cp2 unit Content.component unit absurd ]
  where f (Bar.LangChanged l _) = LangChanged l unit
        f (Bar.TabChanged t _)  = TabChanged t unit

eval :: forall s m. Query ~> H.ParentDSL s Query ChildQuery ChildSlot Void m
eval = case _ of
  LangChanged l next -> H.query' CP.cp2 unit (Content.LangChanged l unit) *> pure next
  TabChanged t next  -> H.query' CP.cp2 unit (Content.TabChanged t unit)  *> pure next
