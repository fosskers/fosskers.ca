module LangToggle ( component, Query(..) ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Language(..), defaultLang)

---

data Query a = Toggle Language a

type State = { language :: Language }

component :: forall m. H.Component HH.HTML Query Unit Language m
component = H.component { initialState: const { language: defaultLang }
                        , render
                        , eval
                        , receiver: const Nothing }

render :: State -> H.ComponentHTML Query
render state = HH.div [ HP.class_ $ H.ClassName "btn-group"
                      , HP.attr (H.AttrName "role") "group"
                      , HP.attr (H.AttrName "aria-label") "Language Toggle" ]
               [ button English "English", button Japanese "日本語" ]
  where button l t = HH.button [ HP.classes $ map H.ClassName [ "btn", colour l ]
                               , HP.attr (H.AttrName "type") "button"
                               , HE.onClick (HE.input_ $ Toggle l) ]
                     [ HH.text t ]
        colour l | l == state.language = "btn-secondary"
                 | otherwise = "btn-outline-secondary"

eval :: forall m. Query ~> H.ComponentDSL State Query Language m
eval = case _ of
  Toggle lang next -> do
    curr <- H.gets _.language
    unless (lang == curr) $ do
      H.modify (_ { language = lang })
      H.raise lang
    pure next
