{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main ) where

import Common
import Language.PureScript.Bridge
import Protolude
import Servant.PureScript
import Time.Types

---

psMonth :: PSType
psMonth = TypeInfo { _typePackage    = "purescript-datetime"
                   , _typeModule     = "Data.Date"
                   , _typeName       = "Month"
                   , _typeParameters = [] }

monthBridge :: BridgePart
monthBridge = typeName ^== "Month" >> pure psMonth

-- | Generic representations of the Haskell types I want to convert to Purescript.
types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy Blog)
        , mkSumType (Proxy :: Proxy Title)
        , mkSumType (Proxy :: Proxy Date)
        , mkSumType (Proxy :: Proxy Path) ]

main :: IO ()
main = do
  writePSTypes "site/src" (buildBridge $ defaultBridge <|> monthBridge) types
  writeAPIModule "site/src" defaultBridgeProxy (Proxy :: Proxy JsonAPI)
