module Bootstrap where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements (Node)
import Halogen.HTML.Properties as HP

---

fluid :: forall p i. Node HTMLdiv p i
fluid as hs = HH.div ([ HP.class_ $ H.ClassName "container-fluid" ] <> as) hs

container :: forall p i. Node HTMLdiv p i
container as hs = HH.div ([ HP.class_ $ H.ClassName "container" ] <> as) hs

-- | A simple shorthand for Bootstrap grid containers.
container_ :: forall t1 t2. Array (HH.HTML t2 t1) -> HH.HTML t2 t1
container_ = container []

row :: forall p i. Node HTMLdiv p i
row as hs = HH.div ([ HP.class_ $ H.ClassName "row" ] <> as) hs

-- | A simple shorthand for Bootstrap grid rows.
row_ :: forall t1 t2. Array (HH.HTML t2 t1) -> HH.HTML t2 t1
row_ = row []

col :: forall p i. Node HTMLdiv p i
col as hs = HH.div ([ HP.class_ $ H.ClassName "col" ] <> as) hs

-- | A simple shorthand for Bootstrap grid columns.
col_ :: forall t1 t2. Array (HH.HTML t2 t1) -> HH.HTML t2 t1
col_ = col []

colN :: forall p i. Int -> Node HTMLdiv p i
colN n as hs = HH.div ([ HP.class_ <<< H.ClassName $ "col-" <> show n ] <> as) hs
