module Fosskers.Site.AlBhed ( alBhed ) where

import BasePrelude
import Data.Text (Text)
import Fosskers.Common (Language(..))
import Lucid hiding (col_)
import Lucid.Base (termRawWith)

---

alBhed :: Language -> Html ()
alBhed lang = div_ [class_ "grid-main"] $ div_ [class_ "content"] $ do
  heading
  div_ [class_ "al-bhed-inputs"]$ do
    div_ [classes_ []] $ do
      h3_ engLabel
      form "english-text" engPlace
    div_ [classes_ []] $ do
      h3_ alLabel
      form "al-bhed-text" alPlace
  wasm
  where
    form :: Text -> Text -> Html ()
    form label ph = textarea_ [class_ "textarea", id_ label, placeholder_ ph] ""

    (engLabel, engPlace, alLabel, alPlace) = case lang of
      English -> ("English", "Enter English here", "Al Bhed", "Enter Al Bhed here")
      Japanese -> ("英語", "英語入力", "アルベド語", "アルベド語入力")

    heading :: Html ()
    heading = case lang of
      English -> do
        h1_ [classes_ ["title", "is-centered"]] "Al Bhed Translator"
        p_ "Al Bhed is the language of the Al Bhed people in the land of Spira, the setting for the game Final Fantasy X."
        p_ "Type in either box to see the translated result in the other."

      Japanese -> do
        h1_ [classes_ ["title", "is-centered"]] "アルベド翻訳"
        p_ "アルベド語はファイナルファンタジーXの舞台となる「スピラ界」のアルベド族が話す言語です。"
        p_ "欄に入力すると、その片方の欄に翻訳が出力されます。"

wasm :: Html ()
wasm = termRawWith "script" [type_ "module"]
  "import init from '/assets/code/al_bhed.js';\n\
  \async function run() {\n\
  \  await init();\n\
  \}\n\
  \run();"
