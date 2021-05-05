module Fosskers.Site.GameOfLife ( gol ) where

import BasePrelude
import Fosskers.Common (Language(..))
import Lucid
import Lucid.Base (termRawWith)

---

-- This WASM loading magic is borrowed from:
-- https://rustwasm.github.io/docs/wasm-bindgen/examples/without-a-bundler.html
gol :: Language -> Html ()
gol lang = div_ [class_ "grid-main"] $ div_ [class_ "game-of-life"] $ do
  case lang of
    English  -> english
    Japanese -> japanese
  div_ [classes_ ["buttons", "has-addons"]] $ do
      button_ [classes_ ["button", "is-outlined", "is-warning"], id_ "pause-button"] "Pause"
      button_ [classes_ ["button", "is-outlined", "is-info"], id_ "reset-button"] "Reset"
  canvas_ [id_ "game-of-life-canvas"] ""
  wasm
  where
    english :: Html ()
    english = do
      p_ $ do
        "This is the "
        a_ [href_ "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"] "Game of Life"
        " written entirely in "
        a_ [href_ "https://www.rust-lang.org/"] "Rust"
        " with its "
        a_ [href_ "https://rustwasm.github.io/docs/book/"] "support for Web Assembly."
      p_ $ do
        "The grid itself is an HTML5 "
        code_ "<canvas>"
        " element written to directly from Rust."

    japanese :: Html ()
    japanese = do
      p_ $ do
        "Conwayの"
        a_ [href_ "https://ja.wikipedia.org/wiki/%E3%83%A9%E3%82%A4%E3%83%95%E3%82%B2%E3%83%BC%E3%83%A0"] "ライフゲーム"
        "です。コードはJavascriptを一切使わずに全て"
        a_ [href_ "https://www.rust-lang.org/"] "Rust"
        "とその"
        a_ [href_ "https://rustwasm.github.io/docs/book/"] "Web Assemblyバックエンド"
        "で作られています。"
      p_ $ do
        "盤自体はRustから直接書き込むHTML5の"
        code_ "<canvas>"
        "です。"

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/game_of_life.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
