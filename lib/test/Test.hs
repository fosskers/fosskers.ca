module Main where

import           Common (Title(Title))
import qualified Data.Text as T
import           Org
import           Protolude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec (parse)
import           Text.Megaparsec.Error (parseErrorPretty)
import           Time.Types

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup ".org parsing"
  [ testCase "English" $ case parse org "test org" orgT of
      Left e  -> assertFailure $ parseErrorPretty e
      Right r -> r @?= (Title "Cool Article", Date 2018 January 5)
  ]

orgT :: Text
orgT = T.unlines
  [ "#+TITLE: Cool Article"
  , "#+DATE: 2018-01-05"
  , "#+AUTHOR: Colin"
  , "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"org-theme.css\"/>"
  , ""
  , "hi there" ]
