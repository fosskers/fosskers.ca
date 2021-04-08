module Fosskers.Site.WebEffects ( webEffects ) where

import BasePrelude
import Fosskers.Common (Language(..))
import Lucid
import Lucid.Base (termRawWith)

---

webEffects :: Language -> Html ()
webEffects lang = div_ [class_ "grid-main"] $ div_ [class_ "web-effects"] $ do
  h1_ [classes_ ["title", "is-1"]] "Web Effects"
  p_ [classes_ ["subtitle", "is-5"]] "Various browser interaction samples."
  div_ $ do
    button_ [class_ "button", id_ "print-button"] "Print this Page"
  wasm

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/web_effects.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
