module Kanji where

import Prelude

import Bootstrap (col_, fluid, row_)
import CSS (paddingTop, pct)
import Data.Generic (gShow)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Fosskers.Kanji (Analysis)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import ServerAPI (postKanji)
import Types (Effects, Language, defaultLang, update)

---

data Query a = LangChanged Language a | Update String a

type State = { language :: Language, result :: Maybe Analysis }

component :: forall e. H.Component HH.HTML Query Language Void (Effects e)
component = H.component { initialState: const { language: defaultLang, result: Nothing }
                        , render
                        , eval
                        , receiver: HE.input LangChanged }

render :: State -> H.ComponentHTML Query
render s = fluid [ HC.style <<< paddingTop $ pct 1.0 ]
           [ row_
             [ col_
               [ HH.div [ HP.class_ (H.ClassName "input-group") ]
                 [ HH.div [ HP.class_ (H.ClassName "input-group-prepend") ]
                   [ HH.span [ HP.class_ (H.ClassName "input-group-text") ] [ HH.text "Input" ]]
                 , HH.textarea [ HP.class_ (H.ClassName "form-control")
                               , HP.attr (H.AttrName "aria-label") "Input"
                               , HE.onValueInput (\str -> Just $ Update str unit) ]
                 ]
               ]
             ]
           , row_
             [ col_
               [ HH.text $ gShow s.result ]
             ]
           ]

eval :: forall e. Query ~> H.ComponentDSL State Query Void (Effects e)
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  Update "" next -> H.modify (_ { result = Nothing }) *> pure next
  Update s next -> do
    _ <- HQ.fork do
      a <- H.lift $ postKanji s
      H.modify (_ { result = Just a })
    pure next
