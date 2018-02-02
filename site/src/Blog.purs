module Blog ( component, Query(..) ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Search as Search
import Types (Language(English))

---

data Query a = LangChanged Language a | NewKeywords (Array String) a | Selected String a

type State = { language :: Language, options :: Array Post, selected :: Maybe String }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type Post = { title :: String, filename :: String }

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.parentComponent { initialState: const { language: English , options: mempty , selected: Nothing }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render state = HH.div_ $ [ search ] <> choices <> [ post ]
  where search  = HH.div_ [ HH.slot SearchSlot Search.component state.language (HE.input NewKeywords) ]
        post    = maybe (HH.div_ []) (\s -> HH.iframe [ HP.src $ "../blog/" <> s <> ".html" ]) state.selected
        choices = map f state.options
        f p     = HH.a [ HP.href "#", HE.onClick $ const (Just $ Selected p.filename unit) ] [ HH.h3_ [ HH.text p.title ] ]

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
