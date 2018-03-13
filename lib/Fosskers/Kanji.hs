{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Fosskers.Kanji where

import           ClassyPrelude
import           Data.Aeson
import           Data.Kanji
import qualified Data.Set as S
import qualified Data.Text as T

---

data Analysis = Analysis { levelSplit    :: Map Level (Set Kanji)  -- ^ Unique `Kanji` that appeared in each `Level`.
                         , elementary    :: Float  -- ^ Percent (<= 1.0) of `Kanji` learned in Elementary school.
                         , average       :: Float  -- ^ Average `Level` of all `Kanji` present.
                         , density       :: Float  -- ^ Fraction (<= 1.0) of the input `Char`s that were `Kanji`.
                         , unknowns      :: Set Kanji  -- ^ `Kanji` whose `Level` can't be determined.
                         , distributions :: Map Level Float  -- ^ Fractions of input `Kanji` that belong to each `Level`.
                         } deriving (Generic, ToJSON)

analysis :: Text -> Analysis
analysis t = Analysis (uniques ks) (elementaryDen dist) (averageLevel ks) den uns dist
  where ks   = mapMaybe kanji $ unpack t
        dist = levelDist ks
        uns  = S.filter (isNothing . level) $ S.fromList ks
        den  = kanjiDensity (T.length t) ks
