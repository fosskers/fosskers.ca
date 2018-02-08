module Blog ( component, Query(..) ) where

import Prelude

import Common (_Path, _Title)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (catMaybes, filter, sortWith)
import Data.Foldable (any, null)
import Data.Lens ((^.))
import Data.Lens.Record (prop)
import Data.Map (member)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Network.HTTP.Affjax (AJAX)
import Search as Search
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_)
import ServerAPI (SPParams_, getPosts)
import Types (Language(Japanese, English), Post, asPost, update)

---

data Query a = LangChanged Language a
             | NewKeywords (Array String) a
             | Selected String a
             | Initialize a

type State = { language :: Language, options :: Array Post, keywords :: Array String, selected :: Maybe String }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e m.
  MonadAff (ajax :: AJAX | e) m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadError AjaxError m =>
  H.Component HH.HTML Query Unit Void m
component = H.lifecycleParentComponent { initialState: const state
                                       , render
                                       , eval
                                       , receiver: const Nothing
                                       , initializer: Just $ Initialize unit
                                       , finalizer: Nothing }
  where state = { language: English, options: mempty, keywords: mempty, selected: Nothing }

        eval :: Query ~> H.ParentDSL State Query Search.Query Slot Void m
        eval = case _ of
          LangChanged l next   -> update (prop (SProxy :: SProxy "language")) l *> pure next
          NewKeywords kws next -> update (prop (SProxy :: SProxy "keywords")) kws *> pure next
          Selected s next      -> update (prop (SProxy :: SProxy "selected")) (Just s) *> pure next
          Initialize next      -> do
            _ <- HQ.fork do
              posts <- H.lift getPosts
              H.modify (_ { options = catMaybes $ map asPost posts })
            pure next

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render s = HH.div_ $ [ search ] <> choices s <> [ post s ]
  where search = HH.div_ [ HH.slot SearchSlot Search.component s.language (HE.input NewKeywords) ]

post :: forall c q. State -> HH.HTML c q
post state = maybe (HH.div_ []) f state.selected
  where f s = HH.iframe [ HP.src $ "../blog/" <> s <> postfix <> ".html" ]
        postfix = case state.language of
          English  -> ""
          Japanese -> "-jp"

-- | If no keywords, rank by date. Otherwise, rank by "search hits".
choices :: forall c. State -> Array (HH.HTML c (Query Unit))
choices state = map f options
  where f p = let title = case state.language of
                    English  -> p.engTitle ^. _Title
                    Japanese -> p.japTitle ^. _Title
                  fname = p.filename ^. _Path
              in HH.a [ HP.href "#", HE.onClick $ const (Just $ Selected fname unit) ] [ HH.h3_ [ HH.text title ] ]
        g p = any (\kw -> member kw p.freqs) state.keywords
        options | null state.keywords = sortWith (_.date) state.options
                | otherwise = filter g state.options  -- TODO Sort by hits
