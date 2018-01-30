module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

---

message :: forall a. H.Component HH.HTML Identity Unit Unit a
message = H.component { initialState: const unit
                      , render
                      , eval
                      , receiver: const Nothing }

render :: forall q s. s -> H.ComponentHTML q
render _ = HH.text "hi"

eval :: forall s a. Identity ~> H.ComponentDSL s Identity Unit a
eval = case _ of
  Identity next -> pure next

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI message unit body
