module Org where

import           Common (Title(..))
import           Data.Bifunctor (first)
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Time.Types

---

type Parser = Parsec Void Text

english :: Parser (Title, Date)
english = (,) <$> title <*> date <* takeRest

japanese :: Parser Title
japanese = title <* takeRest

title :: Parser Title
title = Title . toS <$> (string "#+TITLE: " *> someTill anyChar newline)

date :: Parser Date
date = do
  string "#+DATE: "
  year  <- L.decimal <* char '-'
  month <- fmap (toEnum . pred) L.decimal <* char '-'
  day   <- L.decimal
  pure $ Date year month day

parseEng :: Text -> Either Text (Title, Date)
parseEng = first (toS . parseErrorPretty) . parse english "English ORG file"

parseJap :: Text -> Either Text Title
parseJap = first (toS . parseErrorPretty) . parse japanese "Japanese ORG file"
