module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Fosskers.Common (Path(..))
import Servant.PureScript.Settings (SPSettingsDecodeJson_(..), SPSettingsEncodeJson_(..), SPSettings_(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Types (Language(..), postLang, settings)

---

main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e ) Unit
main = runTest do
  suite "Unit Tests" do
    test "Language Detection" do
      Assert.assert "deploying-haskell-en should be detected as English"  $ postLang epath == English
      Assert.assert "deploying-haskell-jp should be detected as Japanese" $ postLang jpath == Japanese
    -- test "JSON Codec" do
    --   Assert.assert "KText Isomorphism" $ Right ktext == decoder (encoder ktext)

epath :: { path :: Path }
epath = { path: Path "deploying-haskell-en" }

jpath :: { path :: Path }
jpath = { path: Path "deploying-haskell-jp" }

opts = case settings of
  SPSettings_ o -> o

encoder = case opts.encodeJson of
  SPSettingsEncodeJson_ e -> e

decoder = case opts.decodeJson of
  SPSettingsDecodeJson_ e -> e
