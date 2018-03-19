module Kanji where

import Prelude

import Bootstrap (col_, container, row, row_)
import CSS (paddingTop, pct)
import Data.Array (null)
import Data.Formatter.Number (Formatter(..), format)
import Data.Generic (gShow)
import Data.Int (round)
import Data.Kanji.Types (CharCat(..))
import Data.Lens ((^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String as S
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import Fosskers.Kanji (Analysis(..), _Analysis)
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
render s = container [ HC.style <<< paddingTop $ pct 1.0 ] $
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
           ] <> maybe [] charts s.analysis
  where f = Formatter { comma: false, before: 2, after: 2, abbreviations: false, sign: false }
        charts a = [
          row [ HC.style <<< paddingTop $ pct 1.0 ]
          [ col_ [ density $ a ^. _Analysis <<< prop (SProxy :: SProxy "density") ]]
          , row [ HC.style <<< paddingTop $ pct 1.0 ]
            [ HH.div [ HP.classes $ map HH.ClassName [ "col-xs-12", "col-md-6" ]]
              [ HH.slot 0 (EC.echarts Nothing) ({ width: 500, height: 350 } /\ unit)
                (Just <<< H.action <<< HandleEChartsMsg)
               ]
            , HH.div [ HP.classes $ map HH.ClassName [ "col-xs-12", "col-md-6" ]]
              [ HH.slot 1 (EC.echarts Nothing) ({ width: 500, height: 350 } /\ unit)
                (Just <<< H.action <<< HandleEChartsMsg)
              ]
            ]
          ]

density :: forall t340 t341. Array (Tuple CharCat Number) -> HH.HTML t341 t340
density d = HH.div [ HP.class_ $ H.ClassName "progress"
                   , HP.attr (H.AttrName "style") "height: 20px;" ] $
            map f d
  where f (Tuple c n) =
          let Tuple colour label = g c
              v = show <<< round $ n * 100.0
              v' = format form (n * 100.0)
          in HH.div [ HP.classes $ map H.ClassName [ "progress-bar", "progress-bar-striped", colour ]
                    , HP.attr (H.AttrName "role") "progressbar"
                    , HP.attr (H.AttrName "style") ("width: " <> v <> "%")
                    , HP.attr (H.AttrName "aria-valuenow") v
                    , HP.attr (H.AttrName "aria-valuemin") "0"
                    , HP.attr (H.AttrName "aria-valuemax") "100" ]
             [ HH.text $ v' <> "% " <> label ]
        g Hanzi    = Tuple "bg-info" "Kanji"
        g Hiragana = Tuple "bg-success" "Hiragana"
        g Katakana = Tuple "bg-warning" "Katakana"
        g Other    = Tuple "bg-secondary" "Non-Japanese"
        form = Formatter { comma: false, before: 2, after: 1, abbreviations: false, sign: false }

-- これは日本語串糞猫牛虎山羊森林一二三

eval :: forall e. Query ~> H.ParentDSL State Query EC.EChartsQuery Slot Void (Effects' ( ajax :: AJAX | e ))
eval = case _ of
  LangChanged l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  Update "" next -> H.modify (_ { analysis = Nothing }) *> pure next  -- TODO Should clear charts
  Update s next -> do
    _ <- HQ.fork do
      a <- H.lift $ postKanji s
      H.modify (_ { analysis = Just a })
      when (hasRealData a) do
        void $ H.query 0 $ H.action $ EC.Set $ interpret $ byLevel a
        void $ H.query 1 $ H.action $ EC.Set $ interpret $ lifeStages a
    pure next
  HandleEChartsMsg EC.Initialized next -> do
    a <- H.gets _.analysis
    case a of
      Nothing -> pure unit
      Just a' -> when (hasRealData a') do
        void $ H.query 0 $ H.action $ EC.Set $ interpret $ byLevel a'
        void $ H.query 1 $ H.action $ EC.Set $ interpret $ lifeStages a'
    pure next
  HandleEChartsMsg (EC.EventRaised evt) next -> pure next

hasRealData :: Analysis -> Boolean
hasRealData (Analysis a) = not $ null a.distributions

byLevel :: Analysis -> DSL' ETP.OptionI
byLevel (Analysis a) = do
  E.tooltip do
    E.trigger ET.ItemTrigger
    E.formatterString "{b} <br /> {d}%"
  E.title do
    E.text "Level Distributions"
    E.subtext "Percentage of Kanji per Level"
  E.series do
    E.pie do
      E.name "Levels"
      E.center $ ET.Point { x: ET.Percent 50.0, y: ET.Percent 60.0 }
      E.selectedMode ET.Single
      E.buildItems $
        traverse_ (\(Tuple l n) -> E.addItem $ E.value n *> E.name (S.drop 17 $ gShow l)) a.distributions

lifeStages :: Analysis -> DSL' ETP.OptionI
lifeStages (Analysis a) = do
  E.tooltip do
    E.trigger ET.ItemTrigger
    E.formatterString "{b} <br /> {d}%"
  E.title do
    E.text "Kanji by Life Stages"
    E.subtext "Percentage of Kanji learned by different life stages"
  E.series do
    E.pie do
      E.name "Kanji"
      E.center $ ET.Point { x: ET.Percent 50.0, y: ET.Percent 60.0 }
      E.selectedMode ET.Single
      E.buildItems do
        E.addItem $ E.value a.elementary *> E.name "Elementary Shcool"
        E.addItem $ E.value (a.middle - a.elementary) *> E.name "Middle School"
        E.addItem $ E.value (a.high - a.middle) *> E.name "High School"
        E.addItem $ E.value (a.adult - a.high) *> E.name "Adult"
        E.addItem $ E.value (1.0 - a.adult) *> E.name "Higher"
