{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Main ( main ) where

import BasePrelude
import Data.Kanji.Types
import Data.Proxy (Proxy(..))
import Fosskers.Common
import Fosskers.Kanji
import Language.PureScript.Bridge hiding (Language)
import Language.PureScript.Bridge.PSTypes
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

floatBridge :: BridgePart
floatBridge = typeName ^== "Float" >> pure psNumber

charBridge :: BridgePart
charBridge = typeName ^== "Char" >> pure psChar

psChar :: PSType
psChar = TypeInfo { _typePackage    = "purescript-prim"
                  , _typeModule     = "Prim"
                  , _typeName       = "Char"
                  , _typeParameters = [] }

-- | Generic representations of the Haskell types I want to convert to Purescript.
types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy Blog)
        , mkSumType (Proxy :: Proxy Title)
        , mkSumType (Proxy :: Proxy Date)
        , mkSumType (Proxy :: Proxy Path)
        , mkSumType (Proxy :: Proxy Analysis)
        , mkSumType (Proxy :: Proxy Kanji)
        , let p = (Proxy :: Proxy Level) in order p $ mkSumType p
        , let p = (Proxy :: Proxy CharCat) in order p $ mkSumType p
        , let p = (Proxy :: Proxy Language) in order p $ mkSumType p
        ]

main :: IO ()
main = do
  writePSTypes "site/src" (buildBridge $ defaultBridge <|> monthBridge <|> floatBridge <|> charBridge) types
  writeAPIModule "site/src" defaultBridgeProxy (Proxy :: Proxy JsonAPI)
