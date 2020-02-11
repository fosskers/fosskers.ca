module Fosskers.Org ( parseOrg ) where

import           Data.Bifunctor (first)
import           Fosskers.Common (Title(..))
import           RIO hiding (first)
import           RIO.Partial (pred, toEnum)
import qualified RIO.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Time.Types

---

type Parser = Parsec Void Text

org :: Parser (Title, Date)
org = (,) <$> title <*> date <* takeRest

title :: Parser Title
title = Title . T.pack <$> (string "#+TITLE: " *> someTill anySingle newline)

date :: Parser Date
date = do
  void $ string "#+DATE: "
  Date
    <$> (L.decimal <* char '-')
    <*> (fmap (toEnum . pred) L.decimal <* char '-')
    <*> L.decimal

parseOrg :: Text -> Text -> Either Text (Title, Date)
parseOrg fp t = first (T.pack . errorBundlePretty) $ parse org (T.unpack fp) t
