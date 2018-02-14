module Blog ( component, Query(..) ) where

import Prelude

import Bootstrap (colN, col_, container, row, row_)
import CSS (paddingTop, pct)
import Common (Path, _Path, _Title)
import Control.Error.Util (bool)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Classy.Node (class IsNode, appendChild, childNodes, firstChild, lastChild, removeChild)
import DOM.DOMParser (newDOMParser, parseHTMLFromString)
import DOM.Node.NodeList (item, length)
import DOM.Node.Types (Node)
import Data.Array (catMaybes, filter, head, range, reverse, sortWith)
import Data.Foldable (any, intercalate, null)
import Data.Lens ((^.), (^?))
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid (mempty)
import Data.Set as S
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Network.HTTP.Affjax (AJAX, get)
import Search as Search
import ServerAPI (getPosts)
import Types (Effects, Language(Japanese, English), Post, asPost, defaultLang, localizedDate, update)

---

data Query a = LangChanged Language a
             | NewKeywords (S.Set String) a
             | Selected Path a
             | Initialize a

type State = { language :: Language, posts :: Array Post, keywords :: S.Set String, selected :: Maybe String }

data Slot = SearchSlot
derive instance eqSlot  :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall e. H.Component HH.HTML Query Language Void (Effects e)
component = H.lifecycleParentComponent { initialState: const state
                                       , render
                                       , eval
                                       , receiver: HE.input LangChanged
                                       , initializer: Just $ Initialize unit
                                       , finalizer: Nothing }
  where state = { language: defaultLang, posts: mempty, keywords: mempty, selected: Nothing }

render :: forall m. State -> H.ParentHTML Query Search.Query Slot m
render s = container [ HC.style <<< paddingTop $ pct 1.0 ] $ [ search ] <> choices s <> [ post s ]
  where search = row_ [ col_ [ HH.slot SearchSlot Search.component s.language (HE.input NewKeywords) ] ]

xhrtest :: forall e. Aff ( ajax :: AJAX, console :: CONSOLE, dom :: DOM | e ) (Array Node)
xhrtest = do
  log "Before..."
  res <- get "/xhrtest"
  log "After! Parse time..."
  liftEff do
    parser <- newDOMParser
    let doc = parseHTMLFromString res.response parser
    body <- firstChild doc >>= (map join <<< traverse lastChild)
    case body of
      Nothing -> pure []
      Just el -> children el

post :: forall c q. State -> HH.HTML c q
post _ = HH.div [ HP.ref (H.RefLabel "blogpost") ] []

-- post :: forall c q. State -> HH.HTML c q
-- post state = maybe (HH.div_ []) f state.selected
  -- where f s = row_ [ col_  [ HH.a [ HP.attr (H.AttrName "href") $ "assets/" <> s <> postfix <> ".html" ]
        -- postfix = case state.language of
          -- English  -> ""
          -- Japanese -> "-jp"

-- | If no keywords, rank by date. Otherwise, rank by "search hits".
choices :: forall c. State -> Array (HH.HTML c (Query Unit))
choices s = options >>= f
  where f p = let title = case s.language of
                    English  -> p.engTitle ^. _Title
                    Japanese -> p.japTitle ^. _Title
                  hits = case s.language of
                    English  -> "Keyword hits: "
                    Japanese -> "キーワード出現回数：　"
                  matches = map (\(Tuple k v) -> k <> " × " <> show v)
                            <<< reverse <<< sortWith snd <<< M.toUnfoldable $ hitsOnly p
              in [ row [ HC.style <<< paddingTop $ pct 1.0 ]
                   [ col_ [ HH.a [ HP.href "#", HE.onClick $ const (Just $ Selected p.filename unit) ]
                            [ HH.h3_ [ HH.text title ] ] ]
                   ]
                 , row_ $ [ colN 3 [] [ HH.i_ [ HH.text $ localizedDate s.language p.date ] ] ]
                   <> bool [] [ col_ [ HH.b_ [ HH.text hits ]
                                     , HH.text $ intercalate ", " matches ]] (not $ null matches)
                 ]
        g p = any (\kw -> M.member kw p.freqs) s.keywords
        options | null s.keywords = s.posts
                | otherwise = reverse <<< sortWith hitsOnly $ filter g s.posts
        hitsOnly p = M.filterKeys (\k -> S.member k s.keywords) p.freqs

    -- H.lift <<< liftAff <<< log $ "Blog: New keywords -> " <> intercalate " " kws
eval :: forall e. Query ~> H.ParentDSL State Query Search.Query Slot Void (Effects e)
eval = case _ of
  LangChanged l next    -> update (prop (SProxy :: SProxy "language")) l *> pure next
  NewKeywords kws next  -> update (prop (SProxy :: SProxy "keywords")) kws *> pure next
  Selected s next       -> do
    curr <- H.gets _.selected
    let fname = s ^? _Path
    unless (fname == curr) $ do
      H.modify (_ { selected = fname })
      htmls <- H.getHTMLElementRef (H.RefLabel "blogpost")
      traverse_ (\el -> liftAff xhrtest >>= liftEff <<< replaceChildren el) htmls
    pure next
  Initialize next -> do
    _ <- HQ.fork do
      rawPosts <- H.lift getPosts
      let posts = reverse <<< sortWith _.date <<< catMaybes $ map asPost rawPosts
      H.modify (_ { posts = posts })
      maybe (pure unit) (\p -> eval $ Selected p.filename unit) $ head posts
    pure next

replaceChildren :: forall e n m. IsNode n => IsNode m => n -> Array m -> Eff ( dom :: DOM | e ) Unit
replaceChildren el news = removeChildren el *> traverse_ (\n -> appendChild n el) news

removeChildren :: forall n e. IsNode n => n -> Eff ( dom :: DOM | e ) Unit
removeChildren el = children el >>= traverse_ (\n -> removeChild n el)

children :: forall n e. IsNode n => n -> Eff ( dom :: DOM | e ) (Array Node)
children el = do
  kids <- childNodes el
  len  <- length kids
  let ixs = range 0 (len - 1)
  catMaybes <$> traverse (\i -> item i kids) ixs
