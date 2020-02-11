{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Fosskers.Kanji ( Analysis, analysis ) where

import           Data.Aeson (ToJSON)
import           Data.Kanji
import           RIO
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T

---

data Analysis = Analysis
  { unknowns      :: ![Kanji]
    -- ^ `Kanji` whose `Level` couldn't be determined.
  , elementary    :: !Float
    -- ^ Percent (<= 1.0) of `Kanji` learned in Elementary school.
  , middle        :: !Float
    -- ^ Percent (<= 1.0) of `Kanji` learned by the end of Middle School.
  , high          :: !Float
    -- ^ Percent (<= 1.0) of `Kanji` learned by the end of High School.
  , density       :: ![(CharCat, Float)]
    -- ^ Density of each character category in the text.
  , distributions :: ![(Level, Float)]
    -- ^ Fractions of input `Kanji` that belong to each `Level`.
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

analysis :: Text -> Analysis
analysis t
  | T.null t = Analysis [] 0 0 0 [] []
  | otherwise = Analysis uniq (elementaryDen dist) (middleDen dist) (highDen dist) den (M.toList dist)
  where
    ks   = mapMaybe kanji $ T.unpack t
    uniq = fromMaybe [] . M.lookup Unknown . fmap S.toList $ uniques ks
    dist = levelDist ks
    den  = M.toList $ densities t
