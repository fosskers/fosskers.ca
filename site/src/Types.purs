module Types where

import Prelude

import Control.Monad.State (class MonadState, gets, modify)
import Data.Generic (class Generic)
import Data.Lens (Lens', (.~), (^.))

---

data Language = English | Japanese
derive instance eqLanguage :: Eq Language

data Tab = About | Blog
derive instance tabGeneric :: Generic Tab

-- | Updates some State, so long as it hasn't changed.
update :: forall s a m. MonadState s m => Eq a => Lens' s a -> a -> m Unit
update l a = do
  curr <- gets (_ ^. l)
  unless (a == curr) $ modify (_ # l .~ a)
