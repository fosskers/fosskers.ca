module Blog ( component, Query(..) ) where

import Common (Blog, _Blog, _Path, _Title)
import Data.Lens ((^.))
import Prelude

import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Search as Search
import Types (Language(..))

---

data Query a = LangChanged Language a
             | NewKeywords (Array String) a
             | Selected String a
             | NewPosts (Array Blog) a

type State = { language :: Language, options :: Array Blog, selected :: Maybe String }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.parentComponent { initialState: const { language: English, options: mempty, selected: Nothing }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render s = HH.div_ $ [ search ] <> choices s <> [ post s ]
  where search = HH.div_ [ HH.slot SearchSlot Search.component s.language (HE.input NewKeywords) ]

post :: forall c q. State -> HH.HTML c q
post state = maybe (HH.div_ []) f state.selected
  where f s = HH.iframe [ HP.src $ "../blog/" <> s <> postfix <> ".html" ]
        postfix = case state.language of
          English  -> ""
          Japanese -> "-jp"

choices :: forall c. State -> Array (HH.HTML c (Query Unit))
choices state = map f state.options
  where f p = let title = case state.language of
                    English  -> p ^. _Blog <<< prop (SProxy :: SProxy "engTitle") <<< _Title
                    Japanese -> p ^. _Blog <<< prop (SProxy :: SProxy "japTitle") <<< _Title
                  fname = p ^. _Blog <<< prop (SProxy :: SProxy "filename") <<< _Path
              in HH.a [ HP.href "#", HE.onClick $ const (Just $ Selected fname unit) ] [ HH.h3_ [ HH.text title ] ]

eval :: forall m. Query ~> H.ParentDSL State Query Search.Query Slot Void m
eval = case _ of
  LangChanged l next -> do
    lang <- H.gets _.language
    unless (l == lang) $ H.modify (_  { language = l })
    pure next
  NewKeywords kws next -> pure next
  Selected s next  -> do
    curr <- H.gets _.selected
    unless (Just s == curr) $ H.modify (_ { selected = Just s })
    pure next
  NewPosts ps next -> pure next
