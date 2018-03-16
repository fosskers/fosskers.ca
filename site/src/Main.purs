module Main ( main ) where

import Prelude

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import ECharts.Types (ECHARTS)
import Halogen.Aff as HA
import Halogen.Component as HC
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Page as Page
import Types (runEffects)

---

main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX, console :: CONSOLE, echarts :: ECHARTS )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (HC.hoist runEffects Page.component) unit body
