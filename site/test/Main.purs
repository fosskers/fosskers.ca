module Test.Main where

import Prelude

import Common (Path(..))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Types (Language(..), postLang)

---

main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e ) Unit
main = runTest do
  suite "Unit Tests" do
    test "Language Detection" do
      Assert.assert "deploying-haskell-en should be detected as English"  $ postLang epath == English
      Assert.assert "deploying-haskell-jp should be detected as Japanese" $ postLang jpath == Japanese

epath :: { path :: Path }
epath = { path: Path "deploying-haskell-en" }

jpath :: { path :: Path }
jpath = { path: Path "deploying-haskell-jp" }
