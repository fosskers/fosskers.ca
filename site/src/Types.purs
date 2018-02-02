module Types where

import Data.Generic (class Generic)
import Prelude (class Eq)

---

data Language = English | Japanese
derive instance eqLanguage :: Eq Language

data Tab = About | Blog
derive instance tabGeneric :: Generic Tab
