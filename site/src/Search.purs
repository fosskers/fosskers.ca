module Search ( component, Query(..) ) where

import Prelude

import Data.Array (null)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Language(..))

---

data Query a = Update String a | SelectLang Language a

type Input = Language

type State = { keywords :: Array String, language :: Language }

component :: forall m. H.Component HH.HTML Query Input Void m
component = H.component { initialState: const { keywords: mempty, language: English }
                        , render
                        , eval
                        , receiver: HE.input SelectLang }

render :: State -> H.ComponentHTML Query
render state = HH.div_
               [ HH.input [ HP.placeholder label, HE.onValueInput (\s -> Just $ Update s unit) ]
               , HH.text $ intercalate " " state.keywords ]
  where label = case state.language of
          English  -> "Enter keywords to search by."
          Japanese -> "キーワードで検索"

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  Update s next -> do
    curr <- H.gets _.keywords
    -- | Avoiding `modify` calls prevents spurrious rerendering.
    unless (null curr && S.null s) $ H.modify (_ { keywords = S.split (S.Pattern " ") s })
    pure next
  SelectLang l next -> do
    lang <- H.gets _.language
    unless (l == lang) $ H.modify (_ { language = l })
    pure next
