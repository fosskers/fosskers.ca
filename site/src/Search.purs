module Search ( component, Query(..) ) where

import Prelude

import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Set as Set
import Data.String as Str
import Data.Symbol (SProxy(..))
import Fosskers.Common (Language(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (defaultLang, update)

---

data Query a = Update String a | SelectLang Language a

type State = { keywords :: Set.Set String, language :: Language }

component :: forall m. H.Component HH.HTML Query Language (Set.Set String) m
component = H.component { initialState: const { keywords: mempty, language: defaultLang }
                        , render
                        , eval
                        , receiver: HE.input SelectLang }

render :: State -> H.ComponentHTML Query
render state = HH.div_ [ HH.input [ HP.placeholder label
                                  , HP.class_ $ H.ClassName "form-control"
                                  , HE.onValueInput (\s -> Just $ Update s unit) ]]
  where label = case state.language of
          English  -> "Filter posts by keywords"
          Japanese -> "キーワードで検索"

eval :: forall m. Query ~> H.ComponentDSL State Query (Set.Set String) m
eval = case _ of
  Update s next -> do
    state <- H.get
    case state.keywords of
      _ | Str.length s > 2 -> do
        let kws = Set.fromFoldable <<< map Str.toLower $ Str.split (Str.Pattern " ") s
        H.modify (_ { keywords = kws })
        H.raise kws
      curr | not (Set.isEmpty curr) && Str.length s < 3 -> do
        H.modify (_ { keywords = (mempty :: Set.Set String) })
        H.raise mempty
      _ -> pure unit
    pure next
  SelectLang l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
