module Fosskers.Site.Love ( love ) where

import BasePrelude
import Lucid
import Lucid.Base

---

love :: Html ()
love = html_ $ do
  head_ h
  body_ $ do
    section_ [id_ "app"] ""
    wasm
  where
    h :: Html ()
    h = do
      title_ "Love Letter Tracker"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/bootstrap.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/love.css"]

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/love.js';\n\
  \init('/assets/love_bg.wasm');"
