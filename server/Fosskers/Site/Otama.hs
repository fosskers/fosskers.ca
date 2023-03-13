module Fosskers.Site.Otama (otama) where

import BasePrelude
import Lucid
import Lucid.Base

---

otama :: Html ()
otama = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ h
    body_ $ do
      div_ [id_ "amazingtalker"] ""
      termRawWith "script" [] "var app = Elm.Main.init({ node: document.getElementById('amazingtalker') })"

  where
    h :: Html ()
    h = do
      title_ "お珠"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [rel_ "stylesheet", href_ "/assets/css/bulma.min.css"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/assets/images/favicon-16x16.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/assets/images/favicon-32x32.png"]
      termRawWith "script" [src_ "/assets/code/otama.js"] ""
