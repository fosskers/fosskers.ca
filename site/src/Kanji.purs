module Kanji where

import Prelude

import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Types (Effects, Language(..), defaultLang, update)

---

data Query a = LangChanged Language a

type State = { language :: Language }

component :: forall e. H.Component HH.HTML Query Language Void (Effects e)
component = H.component { initialState: const { language: defaultLang }
                        , render
                        , eval
                        , receiver: HE.input LangChanged }

render :: State -> H.ComponentHTML Query
render s = HH.text t
  where t = case s.language of
          English  -> "Kanji page!"
          Japanese -> "漢字"

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
