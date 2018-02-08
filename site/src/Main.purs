module Main ( main ) where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.Component as HC
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Page as Page
import Types (runEffects)

---

main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (HC.hoist runEffects Page.component) unit body
