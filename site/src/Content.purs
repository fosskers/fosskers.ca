module Content ( component, Query(..) ) where

import Prelude

import About (about)
import Blog as Blog
import CSS (display, displayNone)
import Control.Error.Util (bool)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Types (Effects, Language, Tab(About, Blog), defaultLang, defaultTab, update)

---

data Query a = LangChanged Language a | TabChanged Tab a

type State = { language :: Language, tab :: Tab }

-- TODO There will be one slot per possible tab
data Slot = BlogSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component = H.parentComponent { initialState: const { language: defaultLang, tab: defaultTab }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall e. State -> H.ParentHTML Query Blog.Query Slot (Effects e)
render state = HH.div_
               [ HH.div (hide Blog)
                 [ HH.slot BlogSlot Blog.component state.language absurd ]
               , HH.div (hide About) [ about state.language ] ]
  where hide t = bool [ CSS.style $ display displayNone ] [] (t == state.tab)

eval :: forall e. Query ~> H.ParentDSL State Query Blog.Query Slot Void (Effects e)
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  TabChanged t next  -> update (prop (SProxy :: SProxy "tab")) t *> pure next
