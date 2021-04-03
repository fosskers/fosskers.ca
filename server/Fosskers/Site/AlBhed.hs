module Fosskers.Site.AlBhed ( alBhed ) where

import BasePrelude
import Data.Text (Text)
import Fosskers.Common (Language(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)
import Lucid.Base (termRawWith)

---

alBhed :: Language -> Html ()
alBhed lang = do
  heading
  fluid [style_ "padding-top: 1.0%;padding-bottom: 1.0%"] $ row_ $ do
    div_ [classes_ ["col-md-3", "offset-md-3"]] $ do
      div_ [class_ "title"] $ h3_ engLabel
      form "english-text" engPlace
    div_ [classes_ ["col-md-3"]] $ do
      div_ [class_ "title"] $ h3_ alLabel
      form "al-bhed-text" alPlace
  wasm
  where
    form :: Text -> Text -> Html ()
    form label ph = textarea_ [class_ "form-control", id_ label, rows_ "5", placeholder_ ph] ""

    (engLabel, engPlace, alLabel, alPlace) = case lang of
      English -> ("English", "Enter English here", "Al Bhed", "Enter Al Bhed here")
      Japanese -> ("英語", "英語入力", "アルベド語", "アルベド語入力")

    heading :: Html ()
    heading = case lang of
      English -> do
        div_ [class_ "title"] $ do
          h1_ "Al Bhed Translator"
          "Al Bhed is the language of the Al Bhed people in the land of Spira, the setting for the game Final Fantasy X."
        div_ [class_ "title"] "Type in either box to see the translated result in the other."

      Japanese -> do
        div_ [class_ "title"] $ do
          h1_ "アルベド翻訳"
          "アルベド語はファイナルファンタジーXの舞台となる「スピラ界」のアルベド族が話す言語です。"
        div_ [class_ "title"] "欄に入力すると、その片方の欄に翻訳が出力されます。"

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/al_bhed.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
