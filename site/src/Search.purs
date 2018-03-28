module Search ( component, Query(..) ) where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import DOM (DOM)
import DOM.Classy.Element (fromElement, toElement)
import DOM.HTML.HTMLInputElement (setValue)
import Data.Array as A
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Fosskers.Common (Language(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (defaultLang, update)

---

data Query a = Update String a
             | External String a
             | SelectLang Language a

type State = { keywords :: Array String, language :: Language }

component :: forall e m. MonadEff ( dom :: DOM | e ) m => H.Component HH.HTML Query Language (Array String) m
component = H.component { initialState: const { keywords: mempty, language: defaultLang }
                        , render
                        , eval
                        , receiver: HE.input SelectLang }

reflabel :: H.RefLabel
reflabel = H.RefLabel "search-text"

render :: State -> H.ComponentHTML Query
render state = HH.div_ [ HH.input [ HP.placeholder label
                                  , HP.class_ $ H.ClassName "form-control"
                                  , HP.ref reflabel
                                  , HE.onValueInput (\s -> Just $ Update s unit) ]]
  where label = case state.language of
          English  -> "Filter posts by keywords"
          Japanese -> "キーワードで検索"

eval :: forall e m. MonadEff ( dom :: DOM | e ) m => Query ~> H.ComponentDSL State Query (Array String) m
eval = case _ of
  SelectLang l next -> update (prop (SProxy :: SProxy "language")) l *> pure next
  Update s next -> do
    state <- H.get
    case state.keywords of
      _ | Str.length s > 2 -> do
        let kws = map Str.toLower $ Str.split (Str.Pattern " ") s
        H.modify (_ { keywords = kws })
        H.raise kws
      curr | not (A.null curr) && Str.length s < 3 -> do
        H.modify (_ { keywords = [] })
        H.raise []
      _ -> pure unit
    pure next
  External kw next -> do
    kws <- H.gets _.keywords
    e   <- H.getHTMLElementRef reflabel
    case Tuple <$> (e >>= fromElement <<< toElement) <*> A.init kws of
      Nothing -> pure unit
      Just (Tuple e' kws') -> do
        let kws'' = A.snoc kws' kw
        H.liftEff $ setValue (A.intercalate " " kws'') e'
        H.modify (_ { keywords = kws'' })
        H.raise kws''
    pure next
