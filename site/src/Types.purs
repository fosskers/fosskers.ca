module Types where

import Prelude

import Common as C
import Control.Monad.Aff (Aff)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (class MonadState, gets, modify)
import Control.Plus (empty)
import Data.Date as D
import Data.Either (either)
import Data.Enum (toEnum)
import Data.Generic (class Generic)
import Data.Lens (Lens', (.~), (^.))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..))
import Time.Types (Date(..))

---

data Language = English | Japanese
derive instance languageEq :: Eq Language

data Tab = About | Blog
derive instance tabEq :: Eq Tab
derive instance tabGeneric :: Generic Tab

type Post = { engTitle :: C.Title, japTitle :: C.Title, date :: D.Date, filename :: C.Path, freqs :: Map String Int }

-- | Updates some State, so long as it hasn't changed.
update :: forall s a m. MonadState s m => Eq a => Lens' s a -> a -> m Unit
update l a = do
  curr <- gets (_ ^. l)
  unless (a == curr) $ modify (_ # l .~ a)

----------------------
-- EXTRA BRIDGING HELP
----------------------

type Effects eff = ReaderT (SPSettings_ SPParams_) (ExceptT AjaxError (Aff (ajax :: AJAX | eff)))

-- TODO Log the error instead of dumping it?
runEffects :: forall eff. Effects eff ~> Aff (ajax :: AJAX | eff)
runEffects e = runExceptT (runReaderT e settings) >>= either (const empty) pure

settings :: SPSettings_ SPParams_
settings = defaultSettings $ SPParams_ { baseURL: "http://localhost:8080" }

-- | I couldn't find a way to more cleanly "post-process" the bridged-over
-- AssocList into a `Map`. Unfortunately `Map` can't be bridged directly, due
-- to issues with `Generic`.
asPost :: C.Blog -> Maybe Post
asPost (C.Blog b) = map g $ asDate b.date
  where f   = fromFoldable b.freqs
        g d = { engTitle: b.engTitle, japTitle: b.japTitle, date: d, filename: b.filename, freqs: f }

-- | I'd love a more direct bridge between the Haskell and Purescript `Date` types.
asDate :: Date -> Maybe D.Date
asDate (Date d)= (\year day -> D.canonicalDate year d.dateMonth day) <$> toEnum d.dateYear <*> toEnum d.dateDay
