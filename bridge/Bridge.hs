{-# LANGUAGE DataKinds #-}

module Main where

import Language.PureScript.Bridge
import Protolude
import Servant.PureScript
import Types

---

-- | Generic representations of the Haskell types I want to convert to Purescript.
types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy Blog) ]

main :: IO ()
main = do
  writePSTypes "site/src" (buildBridge defaultBridge) types
  writeAPIModule "site/src" defaultBridgeProxy (Proxy :: Proxy API)
