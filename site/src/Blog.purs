module Blog ( component, Query(..) ) where

import Prelude

import Common (_Path, _Title)
import Data.Array (catMaybes, filter, sortWith)
import Data.Foldable (any, null)
import Data.Lens ((^.))
import Data.Lens.Record (prop)
import Data.Map (member)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Search as Search
import ServerAPI (getPosts)
import Types (Effects, Language(Japanese, English), Post, asPost, localizedDate, update)

---

data Query a = LangChanged Language a
             | NewKeywords (Array String) a
             | Selected String a
             | Initialize a

type State = { language :: Language, options :: Array Post, keywords :: Array String, selected :: Maybe String }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
component = H.lifecycleParentComponent { initialState: const state
                                       , render
                                       , eval
                                       , receiver: const Nothing
                                       , initializer: Just $ Initialize unit
                                       , finalizer: Nothing }
  where state = { language: English, options: mempty, keywords: mempty, selected: Nothing }

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render s = HH.div_ $ [ search ] <> choices s <> [ post s ]
  where search = HH.div_ [ HH.slot SearchSlot Search.component s.language (HE.input NewKeywords) ]

post :: forall c q. State -> HH.HTML c q
post state = maybe (HH.div_ []) f state.selected
  where f s = HH.iframe [ HP.src $ "assets/" <> s <> postfix <> ".html" ]
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
              in HH.div_
                 [ HH.a [ HP.href "#", HE.onClick $ const (Just $ Selected fname unit) ]
                        [ HH.h3_ [ HH.text title ] ]
                 , HH.text $ localizedDate state.language p.date ]
        g p = any (\kw -> member kw p.freqs) state.keywords
        options | null state.keywords = sortWith (_.date) state.options
                | otherwise = filter g state.options  -- TODO Sort by hits

eval :: forall e. Query ~> H.ParentDSL State Query Search.Query Slot Void (Effects e)
eval = case _ of
  LangChanged l next   -> update (prop (SProxy :: SProxy "language")) l *> pure next
  NewKeywords kws next -> do
    -- H.lift <<< liftAff <<< log $ "Blog: New keywords -> " <> intercalate " " kws
    update (prop (SProxy :: SProxy "keywords")) kws *> pure next
  Selected s next      -> update (prop (SProxy :: SProxy "selected")) (Just s) *> pure next
  Initialize next      -> do
    _ <- HQ.fork do
      posts <- H.lift getPosts
      H.modify (_ { options = catMaybes $ map asPost posts })
    pure next
