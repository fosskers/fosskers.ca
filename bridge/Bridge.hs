{-# LANGUAGE DataKinds #-}

module Main where

import Language.PureScript.Bridge
import Protolude
import Servant.PureScript
import Types

---

types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy Blog) ]

main :: IO ()
-- main = writeAPIModule "." _ (Proxy :: Proxy API)
main = writePSTypes "frontend" (buildBridge defaultBridge) types
