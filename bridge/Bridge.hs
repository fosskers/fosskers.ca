{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main ) where

import Common
import Language.PureScript.Bridge
import Protolude
import Servant.PureScript
import Time.Types

---

-- | Generic representations of the Haskell types I want to convert to Purescript.
types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy Blog)
        , mkSumType (Proxy :: Proxy Title)
        , mkSumType (Proxy :: Proxy Date)
        , mkSumType (Proxy :: Proxy Path) ]

main :: IO ()
main = do
  writePSTypes "site/src" (buildBridge defaultBridge) types
  writeAPIModule "site/src" defaultBridgeProxy (Proxy :: Proxy API)
