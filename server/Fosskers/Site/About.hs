module Fosskers.Site.About ( about ) where

import Fosskers.Common (Language(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)
import RIO

---

about :: Language -> Html ()
about lang = do
  row [style_ "padding-top: 1.0%"] $ div_ [classes_ ["col-md-6", "offset-md-3"]] content
  row_ $ col_ $ do -- [classes_ ["col-xs-12", "col-md-5", "offset-md-2"]] $ do
    img_ [src_ "/assets/jack.jpg", classes_ ["rounded", "mx-auto", "d-block"]]
    div_ [class_ "text-center"] $ i_ cat
  where
    cat :: Html ()
    cat = case lang of
      English  -> "Jack in a sunbeam."
      Japanese -> "日差しを浴びるジャック"

    content :: Html ()
    content = case lang of
      English  -> english
      Japanese -> japanese

    english :: Html ()
    english = do
      p_ $ do
        "I'm Colin Woodbury, known as Fosskers on the web. "
        "I am professional Haskell developer and am also active in Open Source, with a "
        a_ [href_ "http://hackage.haskell.org/user/fosskers"]
          "number of published applications and libraries. "
        "Functional programming interests me greatly and allows me to realize "
        "my core programming values: Rigour, Minimalism, and Beauty."
      h3_ "Public Projects of Note"
      ul_ $ do
        li_ $ do
          "The "
          a_ [href_ "https://github.com/fosskers/aura"] "Aura Package Manager "
          "for Arch Linux"
        li_ $ do
          "The "
          a_ [href_ "https://github.com/kadena-io/chainweb-node"] "Kadena Public Blockchain"
          " (core developer)"
        li_ $ do
          a_ [href_ "https://github.com/fosskers/mapalgebra"] "MapAlgebra"
          " - An implementation of "
          i_ "GIS and Cartographic Modelling"
          " by Dana Tomlin"
        li_ $ do
          "Mapbox VectorTile codecs ("
          a_ [href_ "https://github.com/fosskers/vectortiles"] "Haskell"
          ", "
          a_ [href_ "https://github.com/locationtech/geotrellis/tree/master/vectortile"]
            "Scala"
          ")"
        li_ $ do
          a_ [href_ "https://geotrellis.github.io/vectorpipe/"] "VectorPipe"
          " (author)"
        li_ $ do
          a_ [href_ "https://github.com/fosskers/scalaz-and-cats"]
            "ScalaZ and Cats Comparison"
          " and the "
          a_ [href_ "https://github.com/fosskers/scala-benchmarks"]
            "Scala Collections Benchmarks"
      p_ $ do
        "Otherwise, I've written code in many languages including Scala, "
        "Purescript, Elm, C, Python, and Java, and deployed projects "
        "to production on AWS, Digital Ocean, and Heroku using a variety of "
        "industry-standard tools and frameworks."
      p_ $ do
        "For more specifics, please see "
        a_ [href_ "/assets/cv.html"] "my full CV."
      p_ $ do
        "Before pursuing programming professionally, I taught English "
        "in Japan for several years. I'm fluent in both English and Japanese, "
        "and have studied German, Italian, and Esperanto. "
        "Apart from learning languages, I often go rock climbing in my spare time."
      p_ $ do
        "This website is written in Haskell using the "
        a_ [href_ "http://hackage.haskell.org/package/servant"] "Servant"
        " and "
        a_ [href_ "http://hackage.haskell.org/package/lucid"] "Lucid"
        " libraries."

    japanese :: Html ()
    japanese = do
      p_ $ do
        "ウッドブリ・コリンと申します。ネット上では「fosskers」という通称で知られています。"
        "ソフト開発者として務めていて、主にHaskellを通して"
        a_ [href_ "http://hackage.haskell.org/user/fosskers"] "複数なライブラリやアプリ"
        "を出しています。"
        "私には関数型プログラミング (Functional Programming) はとても興味深く、"
        "FPによって己の基本的開発美徳の三つが可能となります：　厳密・最小主義・芸術"
      h3_ "主なプロジェクト"
      ul_ $ do
        li_ $ do
          "Arch Linuxの"
          a_ [href_ "https://github.com/fosskers/aura"] "Aura Package Manager"
        li_ $ do
          a_ [href_ "https://github.com/kadena-io/chainweb-node"] "Kadenaの公式ブロックチェーン"
        li_ $ do
          a_ [href_ "https://github.com/fosskers/mapalgebra"] "MapAlgebra"
          "・Dana Tomlin作の「GIS and Cartographic Modelling」の実装"
        li_ $ do
          "Mapbox VectorTileコーデック ("
          a_ [href_ "https://github.com/fosskers/vectortiles"] "Haskell"
          ", "
          a_ [href_ "https://github.com/locationtech/geotrellis/tree/master/vectortile"]
            "Scala"
          ")"
        li_ $ do
          a_ [href_ "https://geotrellis.github.io/vectorpipe/"] "VectorPipe"
        li_ $ a_ [href_ "https://github.com/fosskers/scalaz-and-cats"]
          "ScalaZ and Cats Comparison"
        li_ $ a_ [href_ "https://github.com/fosskers/scala-benchmarks"]
          "Scala Collections Benchmarks"
      p_ $ do
        "他にはPurescript・Elm・Racket・C・Pythonを含めて様々なプログラミング言語"
        "で開発した経験もあります。AWS・Digital Ocean・Herokuなどでシステム管理した事も。"
      p_ $ do
        "職歴の"
        a_ [href_ "/assets/cv-jp.html"] "詳細はこちら"
        "。"
      p_ $ do
        "開発者になる前、数年間長崎県の小中学校で英語教師として働いていました。"
        "英語と日本語は勿論、ドイツ語・イタリア語・エスペラント語も学習しています。"
        "言語の勉強の他、自由時間でよくクライミングをやっています。"
