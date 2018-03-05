module About ( about ) where

import Prelude

import Bootstrap (col_, container, row_)
import CSS (paddingTop, pct)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Types (Language(..), Three(..))

---

about :: forall c q. Language -> HH.HTML c q
about l = container [HC.style <<< paddingTop $ pct 1.0]
        [ row_ [ HH.div [ HP.classes $ map HH.ClassName [ "col-10", "offset-md-1" ]] a ]
        , row_ [ HH.div [ HP.classes $ map HH.ClassName [ "col-xs-12", "col-md-5", "offset-md-1" ]]
                 [ row_ [ col_ [ HH.img [ HP.src "assets/jack.jpg", HP.class_ (HH.ClassName "img-fluid") ]]]
                 , row_ [ col_ [ HH.i_ [ HH.text j ]]]
                 ]
               , HH.div [ HP.classes $ map HH.ClassName [ "col-xs-12", "col-md-5"]]
                 [ row_ [ col_ [ HH.img [ HP.src "assets/qtip.jpg", HP.class_ (HH.ClassName "img-fluid") ]]]
                 , row_ [ col_ [ HH.i_ [ HH.text q ]]]
                 ]
               ]
        ]
  where Three a j q = case l of
          English  -> Three english "Jack in a sunbeam." "Qtip mentoring me on Category Theory."
          Japanese -> Three japanese "日差しに負けるジャック" "キューチップ博士は圏論の教鞭を執る"

english :: forall c q. Array (HH.HTML c q)
english = [ HH.p_
            [ HH.text $ "I'm Colin Woodbury, known as fosskers on the web."
              <> " I program mainly in Haskell, and have "
            , HH.a [ HP.href "http://hackage.haskell.org/user/fosskers" ]
              [ HH.text "a number of published libraries."]
            , HH.text $ " I've also worked professionally in Scala, and am active in both language communities."
              <> " Functional programming interests me greatly and allows me to realize my core"
              <> " programming values: Rigour, Minimalism, and Art."
            ]
          , HH.h3_ [ HH.text "Public Projects of Note" ]
          , HH.ul_
            [ HH.li_ [ HH.text "The "
                     , HH.a [ HP.href "https://github.com/aurapm/aura" ] [ HH.text "Aura Package Manager" ]
                     , HH.text " for Arch Linux" ]
            , HH.li_ [ HH.text "Mapbox VectorTile codecs ("
                     , HH.a [ HP.href "https://github.com/fosskers/vectortiles" ] [ HH.text "Haskell" ]
                     , HH.text ", "
                     , HH.a [ HP.href "https://github.com/locationtech/geotrellis/tree/master/vectortile" ]
                       [ HH.text "Scala" ]
                       , HH.text ")" ]
            , HH.li_ [ HH.a [ HP.href "https://geotrellis.github.io/vectorpipe/" ] [ HH.text "VectorPipe" ]
                     , HH.text " (author)" ]
            , HH.li_ [ HH.a [ HP.href "https://github.com/locationtech/geotrellis" ] [ HH.text "GeoTrellis"]
                     , HH.text " (code contributor and "
                     , HH.a [ HP.href "https://docs.geotrellis.io/en/latest/" ] [ HH.text "docs author" ]
                     , HH.text ")" ]
            , HH.li_ [ HH.a [ HP.href "https://github.com/fosskers/scalaz-and-cats" ]
                       [ HH.text "ScalaZ and Cats Comparison" ]]
            , HH.li_ [ HH.a [ HP.href "https://github.com/fosskers/scala-benchmarks"]
                       [ HH.text "Scala Collections Benchmarks" ]]
            ]
          , HH.p_
            [ HH.text $ "Otherwise, I've written code in many languages including Purescript, Elm, Racket, "
              <> "C, and Python, and deployed projects to production on AWS, Digital Ocean, and Heroku using "
              <> " a variety of industry-standard tools and frameworks." ]
          , HH.p_
            [ HH.text "For more specifics, please see "
            , HH.a [ HP.href "https://stackoverflow.com/cv/colinwoodbury"] [ HH.text "my CV."]]
          , HH.p_
            [ HH.text $ "Before pursuing programming professionally, I taught English in Japan for several years."
            <> " I'm fluent in both English and Japanese, speak German at an intermediate level,"
            <> " and Italian at a beginner level. Apart from learning languages, I also do Bouldering"
            <> " and Lead Climbing in my spare time. I live with my girlfriend Carmen and our two cats, "
            <> " Jack and Qtip." ]
          ]

japanese :: forall c q. Array (HH.HTML c q)
japanese = [ HH.p_
             [ HH.text $ "ウッドブリ・コリンと申します。ネット上では「fosskers」という通称で知られています。"
               <> "基本、Haskellでプログラミングをやっているが前にもScala開発の仕事をしていました。"
               <> "作り上げた"
             , HH.a [ HP.href "http://hackage.haskell.org/user/fosskers" ]
               [ HH.text "librariesは複数"]
             , HH.text $ "でHaskellとScala両方のコミュニティに活躍しています。"
               <> "私には関数型プログラミング (Functional Programming) はとても興味深く、FPを通して己れの基本的"
               <> "開発美徳の三つが叶う：　厳密・最小主義・芸術"
             ]
           , HH.h3_ [ HH.text "主な公開プロジェクト" ]
           , HH.ul_
             [ HH.li_ [ HH.text "Arch Linuxの"
                      , HH.a [ HP.href "https://github.com/aurapm/aura" ] [ HH.text "Aura Package Manager" ]]
             , HH.li_ [ HH.text "Mapbox VectorTileコーデック ("
                      , HH.a [ HP.href "https://github.com/fosskers/vectortiles" ] [ HH.text "Haskell" ]
                      , HH.text ", "
                      , HH.a [ HP.href "https://github.com/locationtech/geotrellis/tree/master/vectortile" ]
                        [ HH.text "Scala" ]
                      , HH.text ")" ]
             , HH.li_ [ HH.a [ HP.href "https://geotrellis.github.io/vectorpipe/" ] [ HH.text "VectorPipe" ]]
             , HH.li_ [ HH.a [ HP.href "https://github.com/locationtech/geotrellis" ] [ HH.text "GeoTrellis"]
                      , HH.text " （コードの寄付・"
                      , HH.a [ HP.href "https://docs.geotrellis.io/en/latest/" ] [ HH.text "docsの創作" ]
                      , HH.text "）" ]
             , HH.li_ [ HH.a [ HP.href "https://github.com/fosskers/scalaz-and-cats" ]
                        [ HH.text "ScalaZとCatsの比べ" ]]
             , HH.li_ [ HH.a [ HP.href "https://github.com/fosskers/scala-benchmarks"]
                        [ HH.text "Scala Collections Benchmarks" ]]
             ]
           , HH.p_
             [ HH.text $ "他にはPurescript・Elm・Racket・C・Pythonを含めて様々なプログラミング言語で開発した経験もあります。"
               <> "AWS・Digital Ocean・Herokuなどにプロジェクト展開した事も。"]
           , HH.p_
             [ HH.text "職歴の"
             , HH.a [ HP.href "https://stackoverflow.com/cv/colinwoodbury"] [ HH.text "詳細はこちら"]
             , HH.text "。"]
           , HH.p_
             [ HH.text $ "開発者になる前、数年間長崎県の小中学校で英語教師として務めていました。英語・日本語を対等に、"
               <> "ドイツ語を中級で"
               <> "イタリア語を初級で会話ができます。言語学習の他、自由時間でボルダリングをよくやっています。"
               <> "彼女とうちの猫二匹と暮らしています。"]
           ]
