module Fosskers.Org where

import           Data.Bifunctor (first)
import           Fosskers.Common (Title(..))
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Time.Types

---

type Parser = Parsec Void Text

org :: Parser (Title, Date)
org = (,) <$> title <*> date <* takeRest

title :: Parser Title
title = Title . toS <$> (string "#+TITLE: " *> someTill anyChar newline)

date :: Parser Date
date = do
  string "#+DATE: "
  year  <- L.decimal <* char '-'
  month <- fmap (toEnum . pred) L.decimal <* char '-'
  day   <- L.decimal
  pure $ Date year month day

parseOrg :: Text -> Text -> Either Text (Title, Date)
parseOrg fp t = first (toS . parseErrorPretty) $ parse org (toS fp) t
