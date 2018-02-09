module Bar ( component, Query(..), Message ) where

import Prelude

import Control.Error.Util (bool)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LangToggle as LangToggle
import Types (Language(..), Tab(..))

---

data Query a = LangChanged Language a | TabChanged Tab a

type Message = Query Unit

type State = { language :: Language }

data Slot = LangSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Message m
component = H.parentComponent { initialState: const { language: English }
                              , render
                              , eval
                              , receiver: const Nothing }

render :: forall m. State -> H.ParentHTML Query LangToggle.Query Slot m
render state = HH.div_
  [ HH.text "fosskers.ca"
  , a About $ bool "自己紹介" "About" (state.language == English)
  , a Blog  $ bool "ブログ" "Blog" (state.language == English)
  , HH.a [ HP.href "https://github.com/fosskers" ] [ HH.text "Github" ]
  , HH.a [ HP.href "https://twitter.com/fosskers" ] [ HH.text "Twitter" ]
  , HH.slot LangSlot LangToggle.component unit (HE.input LangChanged) ]
  where a tab txt = HH.a [ HP.href "#", HE.onClick $ const (Just $ TabChanged tab unit) ] [ HH.text txt ]

eval :: forall m. Query ~> H.ParentDSL State Query LangToggle.Query Slot Message m
eval = case _ of
  LangChanged l next -> H.modify (_ { language = l }) *> H.raise (LangChanged l unit) *> pure next
  TabChanged t next  -> H.raise (TabChanged t unit) *> pure next
