{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import BasePrelude
import Data.Kanji.Types (CharCat, Kanji, Level)
import Fosskers.Common (Blog, JsonAPI, Language, Path, Title)
import Fosskers.Kanji (Analysis)
import Language.PureScript.Bridge hiding (Language)
import Language.PureScript.Bridge.PSTypes
import Servant.PureScript
import Time.Types (Date)

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
types = [ mkSumType (Proxy @Blog)
        , mkSumType (Proxy @Title)
        , mkSumType (Proxy @Date)
        , mkSumType (Proxy @Path)
        , mkSumType (Proxy @Analysis)
        , mkSumType (Proxy @Kanji)
        , let p = (Proxy @Level) in order p $ mkSumType p
        , let p = (Proxy @CharCat) in order p $ mkSumType p
        , let p = (Proxy @Language) in order p $ mkSumType p
        ]

main :: IO ()
main = do
  writePSTypes "site/src" (buildBridge $ defaultBridge <|> monthBridge <|> floatBridge <|> charBridge) types
  writeAPIModule "site/src" defaultBridgeProxy (Proxy :: Proxy JsonAPI)
