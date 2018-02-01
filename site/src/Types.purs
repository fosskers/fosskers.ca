module Types where

import Data.Eq (class Eq)

---

data Language = English | Japanese
derive instance eqLanguage :: Eq Language
