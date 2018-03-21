module Kanji where

import Prelude

import Bootstrap (col_, container, row, row_)
import CSS (paddingTop, pct)
import Control.Error.Util (bool)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Array (null)
import Data.Formatter.Number (Formatter(..), format)
import Data.Generic (gShow)
import Data.Int (ceil)
import Data.Kanji.Types as K
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
import Fosskers.Common (Language(..))
import Fosskers.Kanji (Analysis(Analysis))
import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Network.HTTP.Affjax (AJAX)
import ServerAPI (getKanjiByText, postKanji)
import Types (Effects, Effects', Four(..), defaultLang, update)

---

data Query a = LangChanged Language a
             | Update String a
             | ButtonSelected String a
             | HandleEChartsMsg EC.EChartsMessage a

type State = { language :: Language, analysis :: Maybe Analysis }

type Slot = Int

component :: forall e. H.Component HH.HTML Query Language Void (Effects e)
component = H.parentComponent { initialState: const { language: defaultLang, analysis: Nothing }
                              , render
                              , eval
                              , receiver: HE.input LangChanged }

render :: forall e. State -> H.ParentHTML Query EC.EChartsQuery Slot (Effects e)
render s = container [ HC.style <<< paddingTop $ pct 1.0 ]
           $ preamble s.language <> buttons s.language <> note s.language <> input <> maybe [] withResults s.analysis
  where preamble English =
          [ row_ [ col_
                   [ HH.p_ [ HH.text "This is a demo of my "
                           , HH.a [ HP.href "http://hackage.haskell.org/package/kanji" ] [ HH.text "Kanji" ]
                           , HH.text $ " library for Haskell. Given Japanese text, it divides it into character "
                             <> "categories, and splits all "
                           , HH.a [ HP.href "https://en.wikipedia.org/wiki/Kanji" ] [ HH.text "漢字 (kanji)"]
                           , HH.text " into complexity/rarity "
                           , HH.i_ [ HH.text "Levels" ]
                           , HH.text " as defined by the "
                           , HH.a [ HP.href "http://www.kanken.or.jp/" ]
                             [ HH.text "Japan Kanji Aptitude Testing Foundation."]
                           ]
                   , HH.p_ [ HH.text $ "Try entering text below, or select one of the sample texts"
                             <> " to see how widely each Kanji level is used in real life."
                           ]]]]
        preamble Japanese =
          [ row_ [ col_
                   [ HH.p_ [ HH.text "これは私の"
                           , HH.a [ HP.href "http://hackage.haskell.org/package/kanji" ] [ HH.text "「Kanji」" ]
                           , HH.text "というHaskellライブラリのデモです。日本語を入力すると字が種別に、漢字が"
                           , HH.a [ HP.href "http://www.kanken.or.jp" ] [ HH.text "日本漢字能力検定協会" ]
                           , HH.text "が定義する「級」に分けられます。"
                           ]
                   , HH.p_ [ HH.text $ "欄に日本語を直接打ったり貼ったりするか、下の例文ボタンを押して"
                             <> "どの級の漢字が実際にどれ程使われるかご覧ください。"
                           ]
                   ]
                 ]]
        buttons l =
          [ row_
            [ col_
              [ HH.div [ HP.class_ (H.ClassName "btn-toolbar")
                       , HP.attr (H.AttrName "role") "toolbar"
                       , HP.attr (H.AttrName "aria-label") "Sample Texts" ]
                $ map button
                [ Four "First Group" "primary" "doraemon"
                  $ bool "ウィキペディア・ドラえもん" "Wikipedia: Doraemon" (l == English)
                , Four "Second Group" "warning" "sumo"
                  $ bool "ニュース・朝青龍対白鵬" "News: Asashoryu vs Hakoho" (l == English)
                , Four "Third Group" "success" "rashomon"
                  $ bool "短篇小説・羅生門（大正四年）" "Short Story: Rashomon (1915)" (l == English)
                , Four "Last Group" "info" "iamacat"
                  $ bool "小説・吾輩は猫である（明治三十八年）" "Novel: I am a Cat (1905)" (l == English)
                ]]]]
        button (Four a c f t) = HH.div [ HP.classes $ map H.ClassName ["btn-group", "mr-2"]
                                       , HP.attr (H.AttrName "role") "group"
                                       , HP.attr (H.AttrName "aria-label") a ]
                               [ HH.button [ HP.attr (H.AttrName "type") "button"
                                           , HP.classes $ map H.ClassName [ "btn", "btn-outline-" <> c ]
                                           , HE.onClick (HE.input_ $ ButtonSelected f)
                                           ]
                                 [ HH.text t ]]
        note English  = [ row [ HC.style <<< paddingTop $ pct 1.0] [ col_ [ HH.p_ [ HH.i_ [ HH.text "Note: Kanji of level \"PreOne\" and above will appear as \"Unknown\". "]]]]]
        note Japanese = [ row [ HC.style <<< paddingTop $ pct 1.0] [ col_ [ HH.p_ [ HH.i_ [ HH.text "注意：準一級以上の漢字は「未確定」と示される。"]]]]]
        input = [ row_
                  [ col_
                    [ HH.div [ HP.class_ (H.ClassName "input-group") ]
                      [ HH.div [ HP.class_ (H.ClassName "input-group-prepend") ]
                        [ HH.span [ HP.class_ (H.ClassName "input-group-text") ]
                          [ HH.text $ bool "入力" "Input" (s.language == English) ]]
                      , HH.textarea [ HP.class_ (H.ClassName "form-control")
                                    , HP.attr (H.AttrName "aria-label") "Input"
                                    , HE.onValueInput (\str -> Just $ Update str unit) ]]]]]
        withResults (Analysis a) = charts a <> unknowns a
        charts a = [ row [ HC.style <<< paddingTop $ pct 1.0 ]
                     [ col_ [ density s.language $ a ^. prop (SProxy :: SProxy "density") ]]
                   , row [ HC.style <<< paddingTop $ pct 1.0 ] [ chart 0, chart 1 ]]
        unknowns a | null a.unknowns = []
                   | otherwise = [ HH.hr_ , HH.h5_ [ HH.text "級が未確定の漢字"], HH.div_ (map weblio a.unknowns) ]

chart :: forall t452 m e.
  MonadAff ( echarts :: ET.ECHARTS, dom :: DOM, avar :: AVAR, exception :: EXCEPTION, ref :: REF | e ) m
   => t452 -> HH.HTML (H.ComponentSlot HH.HTML EC.EChartsQuery m t452 (Query Unit)) (Query Unit)
chart n = HH.div [ HP.classes $ map HH.ClassName [ "col-xs-12", "col-md-6" ]]
          [ HH.slot n (EC.echarts Nothing) ({ width: 500, height: 350 } /\ unit)
            (Just <<< H.action <<< HandleEChartsMsg) ]

weblio :: forall t4 t5. K.Kanji -> HH.HTML t5 t4
weblio (K.Kanji k) = HH.a [ HP.href $ "https://weblio.jp/content/" <> k', HP.attr (H.AttrName "style") "float: left;" ] [ HH.h3_ [ HH.text k' ]]
  where k' = S.singleton k

density :: forall t340 t341. Language -> Array (Tuple K.CharCat Number) -> HH.HTML t341 t340
density l d = HH.div [ HP.class_ $ H.ClassName "progress"
                   , HP.attr (H.AttrName "style") "height: 35px;" ] $
            map f d
  where f (Tuple c n) =
          let Tuple colour label = g c
              v = show <<< ceil $ n * 100.0
              v' = format form (n * 100.0)
          in HH.div [ HP.classes $ map H.ClassName [ "progress-bar", "progress-bar-striped", colour ]
                    , HP.attr (H.AttrName "role") "progressbar"
                    , HP.attr (H.AttrName "style") ("width: " <> v <> "%")
                    , HP.attr (H.AttrName "aria-valuenow") v
                    , HP.attr (H.AttrName "aria-valuemin") "0"
                    , HP.attr (H.AttrName "aria-valuemax") "100" ]
             [ HH.text $ v' <> "% " <> label ]
        g K.Hanzi       = Tuple "bg-info" labels.kanji
        g K.Hiragana    = Tuple "bg-success" labels.hira
        g K.Katakana    = Tuple "bg-warning" labels.kata
        g K.Numeral     = Tuple "bg-primary" labels.nums
        g K.RomanLetter = Tuple "bg-info" labels.abcs
        g K.Punctuation = Tuple "bg-dark" labels.punc
        g K.Other       = Tuple "bg-secondary" labels.other
        form = Formatter { comma: false, before: 1, after: 1, abbreviations: false, sign: false }
        labels = case l of
          English -> { kanji: "Kanji", hira: "Hiragana", kata: "Katakana", nums: "Numbers", abcs: "Alphabet", punc: "Punctuation", other: "Other"}
          Japanese -> { kanji: "漢字", hira: "ひらがな", kata: "カタカナ", nums: "数字", abcs: "ローマ字", punc: "句読点", other: "その他" }

type Labels = { kanji :: String, hira :: String, kata :: String, nums :: String, abcs :: String, punc :: String, other :: String }

eval :: forall e. Query ~> H.ParentDSL State Query EC.EChartsQuery Slot Void (Effects' ( ajax :: AJAX | e ))
eval = case _ of
  LangChanged l next -> do
    update (prop (SProxy :: SProxy "language")) l
    a <- H.gets _.analysis
    maybe (pure unit) (updateCharts l) a
    pure next
  Update "" next -> H.modify (_ { analysis = Nothing }) *> pure next
  Update s next -> do
    _ <- HQ.fork do
      a <- H.lift $ postKanji s
      l <- H.gets _.language
      updateCharts l a
      H.modify (_ { analysis = Just a })
    pure next
  HandleEChartsMsg EC.Initialized next -> do
    s <- H.get
    maybe (pure unit) (updateCharts s.language) s.analysis
    pure next
-- (H.gets _.analysis >>= maybe (pure unit) updateCharts) *> pure next
  HandleEChartsMsg (EC.EventRaised evt) next -> pure next
  ButtonSelected t next -> do
    _ <- HQ.fork do
      a <- H.lift $ getKanjiByText t
      l <- H.gets _.language
      maybe (pure unit) (updateCharts l) a
      H.modify (_ { analysis = a })
    pure next

updateCharts :: forall t800 t801 t804 t805. Language -> Analysis -> H.HalogenM t805 t804 EC.EChartsQuery Int t801 t800 Unit
updateCharts l a = when (hasRealData a) do
  void $ H.query 0 $ H.action $ EC.Set $ interpret $ byLevel l a
  void $ H.query 1 $ H.action $ EC.Set $ interpret $ lifeStages l a

hasRealData :: Analysis -> Boolean
hasRealData (Analysis a) = not $ null a.distributions

byLevel :: Language -> Analysis -> DSL' ETP.OptionI
byLevel l (Analysis a) = do
  E.tooltip do
    E.trigger ET.ItemTrigger
    E.formatterString "{b} <br /> {d}%"
  E.title do
    E.text $ bool "級率" "Level Distributions" (l == English)
    E.subtext $ bool "文はどの級の漢字でできたか" "Percentage of Kanji per Level" (l == English)
  E.series do
    E.pie do
      E.name "Levels"
      E.center $ ET.Point { x: ET.Percent 50.0, y: ET.Percent 60.0 }
      E.selectedMode ET.Single
      E.buildItems $
        traverse_ (\(Tuple le n) -> E.addItem $ E.value n *> E.name (label l le)) a.distributions
  where label English le  = S.drop 17 $ gShow le
        label Japanese le = levelJ le

levelJ :: K.Level -> String
levelJ K.Ten = "十級"
levelJ K.Nine = "九級"
levelJ K.Eight = "八級"
levelJ K.Seven = "七級"
levelJ K.Six = "六級"
levelJ K.Five = "五級"
levelJ K.Four = "四級"
levelJ K.Three = "三級"
levelJ K.PreTwo = "準二級"
levelJ K.Two = "二級"
levelJ K.PreOne = "準一級"
levelJ K.One = "一級"
levelJ K.Unknown = "未確定"

lifeStages :: Language -> Analysis -> DSL' ETP.OptionI
lifeStages l (Analysis a) = do
  E.tooltip do
    E.trigger ET.ItemTrigger
    E.formatterString "{b} <br /> {d}%"
  E.title do
    E.text $ bool "人生段階率" "Kanji by Life Stages" (l == English)
    E.subtext
      $ bool "人生のどの段階までにどの級の漢字を学習しているはずか" "Percentage of Kanji learned by different life stages" (l == English)
  E.series do
    E.pie do
      E.name "Kanji"
      E.center $ ET.Point { x: ET.Percent 50.0, y: ET.Percent 60.0 }
      E.selectedMode ET.Single
      E.buildItems do
        E.addItem $ E.value a.elementary *> E.name labels.elem
        E.addItem $ E.value (a.middle - a.elementary) *> E.name labels.mid
        E.addItem $ E.value (a.high - a.middle) *> E.name labels.high
        E.addItem $ E.value (a.adult - a.high) *> E.name labels.adult
        E.addItem $ E.value (1.0 - a.adult) *> E.name labels.above
  where labels = case l of
          English  -> { elem: "Elementary School", mid: "Middle School", high: "High School", adult: "Adulthood", above: "Higher" }
          Japanese -> { elem: "小学校", mid: "中学校", high: "高校", adult: "大学・一般人", above: "その上" }

type LifeStageLabels = { elem :: String, mid :: String, high :: String, adult :: String, above :: String }
