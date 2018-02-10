module Content ( component, Query(..) ) where

import Prelude

import Blog as Blog
import CSS (display, displayNone)
import Control.Error.Util (bool)
import Data.Array (catMaybes)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.Query.HalogenM as HQ
import ServerAPI (getPosts)
import Types (Effects, Language, Post, Tab(Blog, About), asPost, defaultLang, defaultTab, update)

---

data Query a = LangChanged Language a | TabChanged Tab a | Initialize a

type State = { language :: Language, tab :: Tab, posts :: Array Post }

-- TODO There will be one slot per possible tab
data Slot = BlogSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component = H.lifecycleParentComponent { initialState: const state
                                       , render
                                       , eval
                                       , receiver: const Nothing
                                       , initializer: Just $ Initialize unit
                                       , finalizer: Nothing }
  where state = { language: defaultLang, tab: defaultTab, posts: mempty }

render :: forall e. State -> H.ParentHTML Query Blog.Query Slot (Effects e)
render state = HH.div_
               [ HH.div (hide Blog)
                 [ HH.slot BlogSlot Blog.component { language: state.language, posts: state.posts } absurd ]
               , HH.div (hide About) [ HH.text "hi there!" ]
               ]
  where hide t = bool [ CSS.style $ display displayNone ] [] (t == state.tab)

eval :: forall e. Query ~> H.ParentDSL State Query Blog.Query Slot Void (Effects e)
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  TabChanged t next  -> update (prop (SProxy :: SProxy "tab")) t *> pure next
  Initialize next    -> do
    _ <- HQ.fork do
      posts <- H.lift getPosts
      H.modify (_ { posts = catMaybes $ map asPost posts })
    pure next
