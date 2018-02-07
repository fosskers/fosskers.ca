module Blog ( component, Query(..) ) where

import Prelude

import Common (Blog(..), Date, Path, Title, _Path, _Title)
import Data.Array (filter)
import Data.Foldable (any, null)
import Data.Lens ((^.))
import Data.Lens.Record (prop)
import Data.Map (Map, fromFoldable, member)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Search as Search
import Types (Language(..), update)

---

data Query a = LangChanged Language a
             | NewKeywords (Array String) a
             | Selected String a
             | NewPosts (Array Blog) a

type State = { language :: Language, options :: Array Post, keywords :: Array String, selected :: Maybe String }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

type Post = { engTitle :: Title, japTitle :: Title, date :: Date, filename :: Path, freqs :: Map String Int }

-- | I couldn't find a way to more cleanly "post-process" the bridged-over
-- AssocList into a `Map`. Unfortunately `Map` can't be bridged directly, due
-- to issues with `Generic`.
asPost :: Blog -> Post
asPost (Blog b) = { engTitle: b.engTitle, japTitle: b.japTitle, date: b.date, filename: b.filename, freqs: f }
  where f = fromFoldable b.freqs

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.parentComponent { initialState: const state, render, eval, receiver: const Nothing }
  where state = { language: English, options: mempty, keywords: mempty, selected: Nothing }

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render s = HH.div_ $ [ search ] <> choices s <> [ post s ]
  where search = HH.div_ [ HH.slot SearchSlot Search.component s.language (HE.input NewKeywords) ]

post :: forall c q. State -> HH.HTML c q
post state = maybe (HH.div_ []) f state.selected
  where f s = HH.iframe [ HP.src $ "../blog/" <> s <> postfix <> ".html" ]
        postfix = case state.language of
          English  -> ""
          Japanese -> "-jp"

-- | If no keywords, rank by date. Otherwise, rank by "search hits".
choices :: forall c. State -> Array (HH.HTML c (Query Unit))
choices state = map f options
  where f p = let title = case state.language of
                    English  -> p.engTitle ^. _Title
                    Japanese -> p.japTitle ^. _Title
                  fname = p.filename ^. _Path
              in HH.a [ HP.href "#", HE.onClick $ const (Just $ Selected fname unit) ] [ HH.h3_ [ HH.text title ] ]
        g p = any (\kw -> member kw p.freqs) state.keywords
        options | null state.keywords = state.options -- TODO Sort by date
                | otherwise = filter g state.options  -- TODO Sort by hits

eval :: forall m. Query ~> H.ParentDSL State Query Search.Query Slot Void m
eval = case _ of
  LangChanged l next   -> update (prop (SProxy :: SProxy "language")) l *> pure next
  NewKeywords kws next -> update (prop (SProxy :: SProxy "keywords")) kws *> pure next
  Selected s next      -> update (prop (SProxy :: SProxy "selected")) (Just s) *> pure next
  NewPosts ps next     -> H.modify (_ { options = map asPost ps }) *> pure next
