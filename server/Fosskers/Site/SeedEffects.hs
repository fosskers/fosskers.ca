module Fosskers.Site.SeedEffects where

import BasePrelude
import Fosskers.Common (Language(..))
import Lucid
import Lucid.Base (termRawWith)

---

seedEffects :: Language -> Html ()
seedEffects lang = div_ [class_ "grid-main"] $
  div_ [class_ "web-effects"] $ do
    h1_ [classes_ ["title", "is-1"]] title
    p_ [classes_ ["subtitle", "is-5"]] subtitle
    section_ [id_ "app"] ""
    wasm
  where
    (title, subtitle) = case lang of
      English  -> ("Seed Effects", "Various examples of the Seed framework")
      Japanese -> ("Seed作用", "様々なブラウザ作用の見本")

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/seed_effects.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
