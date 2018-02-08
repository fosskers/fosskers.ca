module Content ( component, Query(..) ) where

import Prelude

import Blog as Blog
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import ServerAPI (SPParams_)
import Types (Language, Tab(Blog, About), update)

---

data Query a = LangChanged Language a | TabChanged Tab a

type State = { tab :: Tab }

-- TODO There will be one slot per possible tab
data Slot = BlogSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e m.
  MonadAff (ajax :: AJAX | e) m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadError AjaxError m =>
  H.Component HH.HTML Query Unit Void m
component = H.parentComponent { initialState: const { tab: Blog }
                              , render
                              , eval
                              , receiver: const Nothing }
  where render :: State -> H.ParentHTML Query Blog.Query Slot m
        render state = case state.tab of
          About -> HH.text "hi"
          Blog  -> HH.slot BlogSlot Blog.component unit absurd

eval :: forall m. Query ~> H.ParentDSL State Query Blog.Query Slot Void m
eval = case _ of
  LangChanged l next -> H.query BlogSlot (Blog.LangChanged l unit) *> pure next
  TabChanged t next  -> update (prop (SProxy :: SProxy "tab")) t *> pure next
