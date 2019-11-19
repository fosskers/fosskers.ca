module Fosskers.Org ( parseOrg ) where

import           BasePrelude
import qualified Data.Text as T
import           Fosskers.Common (Title(..))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Time.Types

---

type Parser = Parsec Void T.Text

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

parseOrg :: T.Text -> T.Text -> Either T.Text (Title, Date)
parseOrg fp t = first (T.pack . errorBundlePretty) $ parse org (T.unpack fp) t
