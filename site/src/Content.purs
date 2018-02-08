module Content ( component, Query(..) ) where

import Prelude

import Blog as Blog
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Types (Language, Tab(Blog, About), Effects, update)

---

data Query a = LangChanged Language a | TabChanged Tab a

type State = { tab :: Tab }

-- TODO There will be one slot per possible tab
data Slot = BlogSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component = H.parentComponent { initialState: const { tab: Blog }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall e. State -> H.ParentHTML Query Blog.Query Slot (Effects e)
render state = case state.tab of
  About -> HH.text "hi"
  Blog  -> HH.slot BlogSlot Blog.component unit absurd

eval :: forall m. Query ~> H.ParentDSL State Query Blog.Query Slot Void m
eval = case _ of
  LangChanged l next -> H.query BlogSlot (Blog.LangChanged l unit) *> pure next
  TabChanged t next  -> update (prop (SProxy :: SProxy "tab")) t *> pure next
