module Fosskers.Site.About ( about ) where

import Fosskers.Common (Language(..))
import Fosskers.Site.Bootstrap
import Lucid hiding (col_)
import RIO

---

about :: Language -> Html ()
about _ = do
  row [style_ "padding-top: 1.0%"] $ div_ [classes_ ["col-6", "offset-md-3"]] content
  -- row_ $ div_ [ classes_ [ "col-xs-12", "col-md-5", "offset-md-2" ]] $ do
  --   row_ $ col_ $ img_ [ src_ "/assets/jack.jpg", class_ "img-fluid" ]
  --   row_ $ col_ $ i_ "Jack in a sunbeam."
  where
    content :: Html ()
    content = do
      p_ $ do
        "I'm Colin Woodbury, known as fosskers on the web. "
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
          " by Data Tomlin"
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
          a_ [href_ "https://github.com/locationtech/geotrellis"] "GeoTrellis"
          " (code contributor and "
          a_ [href_ "https://docs.geotrellis.io/en/latest/"] "docs author"
          ")"
        li_ $ a_ [href_ "https://github.com/fosskers/scalaz-and-cats"]
          "ScalaZ and Cats Comparison"
        li_ $ a_ [href_ "https://github.com/fosskers/scala-benchmarks"]
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
        "Apart from learning languages, I also do Bouldering and Lead Climbing "
        "in my spare time."
