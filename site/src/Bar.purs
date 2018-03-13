module Bar ( component, Query(..), Message ) where

import Prelude

import CSS (em, fontSize)
import Control.Error.Util (bool)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LangToggle as LangToggle
import Types (Four(..), Language(..), Tab(..), defaultLang, defaultTab)

---

data Query a = LangChanged Language a | TabChanged Tab a

type Message = Query Unit

type State = { language :: Language, tab :: Tab }

data Slot = LangSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Message m
component = H.parentComponent { initialState: const { language: defaultLang, tab: defaultTab }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall m. State -> H.ParentHTML Query LangToggle.Query Slot m
render state = HH.nav [ HP.classes $ map H.ClassName [ "navbar", "navbar-expand-md", "navbar-dark", "bg-dark" ]]
  [ HH.a [ HP.href "#", HP.class_ (H.ClassName "navbar-brand") ] [ HH.text "fosskers.ca" ]
  , HH.button [ HP.class_ (H.ClassName "navbar-toggler")
              , HP.attr (H.AttrName "type") "button"
              , HP.attr (H.AttrName "data-toggle") "collapse"
              , HP.attr (H.AttrName "data-target") "#navbarLinks"
              , HP.attr (H.AttrName "aria-controls") "navbarLinks"
              , HP.attr (H.AttrName "aria-expanded") "false"
              , HP.attr (H.AttrName "aria-label") "Toggle navigation" ]
    [ HH.span [ HP.class_ (H.ClassName "navbar-toggler-icon")] []]
  , HH.div [ HP.id_ "navbarLinks", HP.classes $ map H.ClassName [ "collapse", "navbar-collapse"]]
    [ HH.div [ HP.class_ $ H.ClassName "navbar-nav" ]
      [ tabSwitch About a
      , tabSwitch Blog  b
      , tabSwitch Kanji k
      , HH.a [ HP.href "https://stackoverflow.com/cv/colinwoodbury"
             , HP.classes $ map H.ClassName [ "nav-item", "nav-link"]]
        [ HH.text c ]
      , icon "https://github.com/fosskers" [ "fab", "fa-github" ]
      , icon "https://twitter.com/fosskers" [ "fab", "fa-twitter" ]
      , icon "mailto:colingw@gmail.com" [ "fas", "fa-envelope" ]
      , icon ("/rss-" <> bool "jp" "en" (state.language == English)) [ "fas", "fa-rss" ]
      ]
    ]
 , HH.slot LangSlot LangToggle.component unit (HE.input LangChanged) ]
  where tabSwitch tab txt = HH.a [ HP.href "#"
                                 , HP.classes $ map H.ClassName [ "nav-item", "nav-link", bool "" "active" (tab == state.tab) ]
                                 , HE.onClick $ const (Just $ TabChanged tab unit) ]
                            [ HH.text txt ]
        icon url i = HH.a [ HP.href url
                          , HP.classes $ map H.ClassName $ i <> [ "nav-item", "nav-link" ]
                          , HC.style <<< fontSize $ em 1.33333 ] []
        Four a b k c = case state.language of
          English  -> Four "About" "Blog" "Kanji" "CV"
          Japanese -> Four "自己紹介" "ブログ" "漢字分析" "履歴書"

eval :: forall m. Query ~> H.ParentDSL State Query LangToggle.Query Slot Message m
eval = case _ of
  LangChanged l next -> H.modify (_ { language = l }) *> H.raise (LangChanged l unit) *> pure next
  TabChanged t next  -> do
    curr <- H.gets _.tab
    unless (t == curr) $ do
      H.modify (_ { tab = t })
      H.raise (TabChanged t unit)
    pure next
