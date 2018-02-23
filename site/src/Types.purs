module Types where

import Prelude

import Common (_Path)
import Common as C
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (class MonadState, gets, modify)
import Control.Plus (empty)
import DOM (DOM)
import Data.Date as D
import Data.Either (either)
import Data.Enum (fromEnum, toEnum)
import Data.Generic (class Generic)
import Data.Lens (Lens', (.~), (^.))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Data.String as S
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..))
import Time.Types (Date(..))

---

data Language = English | Japanese
derive instance languageEq :: Eq Language

defaultLang :: Language
defaultLang = English

suffix :: Language -> String
suffix English  = "-en"
suffix Japanese = "-jp"

postLang :: Post -> Language
postLang p = case takeRight 3 (p.path ^. _Path) of
  "-jp" -> Japanese
  _     -> English

data Tab = About | Blog
derive instance tabEq :: Eq Tab
derive instance tabGeneric :: Generic Tab

defaultTab :: Tab
defaultTab = Blog

type Post = { title :: C.Title, date :: D.Date, path :: C.Path, freqs :: Map String Int }

-- | Updates some State, so long as it hasn't changed.
update :: forall s a m. MonadState s m => Eq a => Lens' s a -> a -> m Unit
update l a = do
  curr <- gets (_ ^. l)
  unless (a == curr) $ modify (_ # l .~ a)

localizedDate :: Language -> D.Date -> String
localizedDate English  d = show (fromEnum $ D.year d) <> " " <> show (D.month d) <> " " <> show (fromEnum $ D.day d)
localizedDate Japanese d = show (fromEnum $ D.year d) <> "年" <> show (fromEnum $ D.month d) <> "月" <> show (fromEnum $ D.day d) <> "日"

localizedPath :: Language -> C.Path -> C.Path
localizedPath l (C.Path p) = C.Path $ dropRight 3 p <> suffix l

-- | Temporarily stolen from the latest version of `Data.String`, since it
-- wasn't included in the version available in the psc packageset.
dropRight :: Int -> String -> String
dropRight i s = S.take (S.length s - i) s

takeRight :: Int -> String -> String
takeRight i s = S.drop (S.length s - i) s

-- | Dealing with the official 3-Tuples is annoying.
data Three a b c = Three a b c

----------------------
-- EXTRA BRIDGING HELP
----------------------

type Effects eff = ReaderT (SPSettings_ SPParams_)
                   (ExceptT AjaxError (Aff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff)))

runEffects :: forall eff. Effects eff ~> Aff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | eff)
runEffects eff = runExceptT (runReaderT eff settings) >>= either (\e -> log (errorToString e) *> empty) pure

settings :: SPSettings_ SPParams_
settings = defaultSettings $ SPParams_ { baseURL: "/" }

-- | I couldn't find a way to more cleanly "post-process" the bridged-over
-- AssocList into a `Map`. Unfortunately `Map` can't be bridged directly, due
-- to issues with `Generic`.
asPost :: C.Blog -> Maybe Post
asPost (C.Blog b) = map g $ asDate b.date
  where g d = { title: b.title, date: d, path: b.filename, freqs: fromFoldable b.freqs }

-- | I'd love a more direct bridge between the Haskell and Purescript `Date` types.
asDate :: Date -> Maybe D.Date
asDate (Date d)= (\year day -> D.canonicalDate year d.dateMonth day) <$> toEnum d.dateYear <*> toEnum d.dateDay
