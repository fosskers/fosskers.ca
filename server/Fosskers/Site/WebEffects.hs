module Fosskers.Site.WebEffects ( webEffects ) where

import BasePrelude
import Fosskers.Common (Language(..))
import Lucid
import Lucid.Base (termRawWith)

---

webEffects :: Language -> Html ()
webEffects lang = div_ [class_ "grid-main"] $ div_ [class_ "web-effects"] $ do
  h1_ [classes_ ["title", "is-1"]] title
  p_ [classes_ ["subtitle", "is-5"]] subtitle
  div_ $ do
    button_ [class_ "button", id_ "print-button"] printPage
  wasm
  where
    (title, subtitle, printPage) = case lang of
      English -> ("Web Effects", "Various browser interaction samples.", "Print this Page")
      Japanese -> ("ウェブ作用", "様々なブラウザ作用の見本", "本ページを印刷")

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/web_effects.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
