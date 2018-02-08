module Search ( component, Query(..) ) where

import Prelude

import Data.Array (null)
import Data.Foldable (intercalate)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String as S
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Language(..), update)

---

data Query a = Update String a | SelectLang Language a

type State = { keywords :: Array String, language :: Language }

component :: forall m. H.Component HH.HTML Query Language (Array String) m
component = H.component { initialState: const { keywords: mempty, language: English }
                        , render
                        , eval
                        , receiver: HE.input SelectLang }

render :: State -> H.ComponentHTML Query
render state = HH.div_
               [ HH.input [ HP.placeholder label, HE.onValueInput (\s -> Just $ Update s unit) ]
               , HH.text $ intercalate " " state.keywords ]
  where label = case state.language of
          English  -> "Enter keywords to search by"
          Japanese -> "キーワードで検索"

eval :: forall m. Query ~> H.ComponentDSL State Query (Array String) m
eval = case _ of
  Update s next -> do
    state <- H.get
    case state.keywords of
      curr | S.length s > 2 -> do
        let kws = S.split (S.Pattern " ") s
        H.modify (_ { keywords = kws })
        H.raise kws
      curr | not (null curr) && S.length s < 3 -> do
        H.modify (_ { keywords = (mempty :: Array String) })
        H.raise mempty
      _ -> pure unit
    pure next
  SelectLang l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
