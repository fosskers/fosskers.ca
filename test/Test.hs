module Main where

import           ClassyPrelude
import qualified Data.Text as T
import           Fosskers.Common (Title(Title))
import           Fosskers.Org (org)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec (errorBundlePretty, parse)
import           Time.Types (Date(..), Month(..))

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup ".org parsing"
  [ testCase "English" $ case parse org "test org" orgT of
      Left e  -> assertFailure $ errorBundlePretty e
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
