module Kanji where

import Prelude

import Bootstrap (col_, fluid, row_)
import CSS (paddingTop, pct)
import Data.Generic (gShow)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import Fosskers.Kanji (Analysis(..))
import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Network.HTTP.Affjax (AJAX)
import ServerAPI (postKanji)
import Types (Language, Effects, Effects', defaultLang, update)

---

data Query a = LangChanged Language a
             | Update String a
             | HandleEChartsMsg EC.EChartsMessage a

type State = { language :: Language, analysis :: Maybe Analysis }

type Slot = Int

component :: forall e. H.Component HH.HTML Query Language Void (Effects e)
component = H.parentComponent { initialState: const { language: defaultLang, analysis: Nothing }
                              , render
                              , eval
                              , receiver: HE.input LangChanged }

render :: forall e. State -> H.ParentHTML Query EC.EChartsQuery Slot (Effects e)
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
           -- , row_
           --   [ col_
           --     [ HH.text $ gShow s.analysis ]
           --   ]
           , row_
             [ col_
               [ HH.slot 0 (EC.echarts Nothing) ({ width: 500, height: 300} /\ unit) (Just <<< H.action <<< HandleEChartsMsg)
               ]
             ]
           ]

eval :: forall e. Query ~> H.ParentDSL State Query EC.EChartsQuery Slot Void (Effects' ( ajax :: AJAX | e ))
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  Update "" next -> H.modify (_ { analysis = Nothing }) *> pure next  -- TODO Should clear charts
  Update s next -> do
    _ <- HQ.fork do
      a <- H.lift $ postKanji s
      H.modify (_ { analysis = Just a })
      void $ H.query 0 $ H.action $ EC.Set $ interpret $ pie a
    pure next
  HandleEChartsMsg EC.Initialized next -> do
    a <- H.gets _.analysis
    case a of
      Nothing -> pure unit
      Just a' -> void $ H.query 0 $ H.action $ EC.Set $ interpret $ pie a'
    pure next
  HandleEChartsMsg (EC.EventRaised evt) next -> pure next

pie :: Analysis -> DSL' ETP.OptionI
pie (Analysis a) = do
  E.tooltip do
    E.trigger ET.ItemTrigger
    E.formatterString "{b} <br /> {d}%"
  E.title do
    E.text "Level Distributions"
    -- E.subtext "Percentage of Kanji learned in elementary school"
  E.series do
    E.pie do
      E.name "Levels"
      E.center $ ET.Point { x: ET.Percent 50.0, y: ET.Percent 60.0 }
      E.selectedMode ET.Single
      E.buildItems $
        traverse_ (\(Tuple l n) -> E.addItem $ E.value n *> E.name (S.drop 17 $ gShow l)) a.distributions
