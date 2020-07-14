module Fosskers.Site.GameOfLife ( gol ) where

import Fosskers.Common (Language(..))
import Lucid
import Lucid.Base (termRawWith)

---

-- This WASM loading magic is borrowed from:
-- https://rustwasm.github.io/docs/wasm-bindgen/examples/without-a-bundler.html
gol :: Language -> Html ()
gol _ = do
  canvas_ [id_ "smile-canvas"] ""
  wasm

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/game_of_life.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
