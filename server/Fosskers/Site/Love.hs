module Fosskers.Site.Love ( love ) where

import BasePrelude
import Lucid
import Lucid.Base

---

love :: Html ()
love = do
  doctype_
  html_ [lang_ "en"] $ do
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
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/fontawesome.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/brands.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/solid.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/love.css"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/assets/images/favicon-16x16.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/assets/images/favicon-32x32.png"]

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/love.js';\n\
  \init('/assets/code/love_bg.wasm');"
