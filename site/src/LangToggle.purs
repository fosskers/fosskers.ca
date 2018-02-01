module LangToggle ( component, Query(..) ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Types (Language(..))

---

data Query a = Toggle Language a

component :: forall m. H.Component HH.HTML Query Unit Language m
component = H.component { initialState: const unit
                        , render
                        , eval
                        , receiver: const Nothing }

render :: forall s. s -> H.ComponentHTML Query
render _ = HH.div_ [ button English "English", button Japanese "日本語" ]
  where button l t = HH.button [ HE.onClick (HE.input_ $ Toggle l) ] [ HH.text t ]

eval :: forall s m. Query ~> H.ComponentDSL s Query Language m
eval = case _ of
  Toggle lang next -> H.raise lang *> pure next
