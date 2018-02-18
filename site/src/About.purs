module About where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

---

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
          , HH.h3_ [ HH.text "Projects of Note" ]
          , HH.ul_
            [ HH.li_ [ HH.text "The Aura Package Manager for Arch Linux" ]
            , HH.li_ [ HH.text "Mapbox VectorTile codecs (Haskell, Scala)" ]
            , HH.li_ [ HH.text "VectorPipe (author)" ]
            , HH.li_ [ HH.text "GeoTrellis (code contributor and docs author)" ]
            , HH.li_ [ HH.text "ScalaZ and Cats Comparison" ]
            , HH.li_ [ HH.text "Scala Benchmarks" ]
            ]
          , HH.p_
            [ HH.text $ "Otherwise, I've written code in many languages including Purescript, Elm, Racket, "
              <> "C, and Python, and deployed projects to production on AWS, Digital Ocean, and Heroku using "
              <> " a variety of industry-standard tools and frameworks." ]
          , HH.p_
            [ HH.text "For more specifics, please see "
            , HH.a [HP.href "https://stackoverflow.com/cv/colinwoodbury"] [ HH.text "my CV."]]
          , HH.p_
            [ HH.text $ "Before pursuing programming professionally, I taught English in Japan for several years."
            <> " I'm fluent in both English and Japanese, speak German at an intermediate level,"
            <> " and Italian at a beginner level. Apart from learning languages, I also do Bouldering"
            <> " and Lead Climbing in my spare time. I live with my girlfriend Carmen and our two cats, "
            <> " Jack and Qtip." ]
          ]

-- Academic Field experience
-- Recommended novels
