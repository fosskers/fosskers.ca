{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Fosskers.Kanji where

import           ClassyPrelude
import           Data.Aeson
import           Data.Kanji
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

---

data Analysis = Analysis { levelSplit    :: [(Level, [Kanji])]  -- ^ Unique `Kanji` that appeared in each `Level`.
                         , elementary    :: Float  -- ^ Percent (<= 1.0) of `Kanji` learned in Elementary school.
                         , middle        :: Float  -- ^ Percent (<= 1.0) of `Kanji` learned by the end of Middle School.
                         , high          :: Float  -- ^ Percent (<= 1.0) of `Kanji` learned by the end of High School.
                         , adult         :: Float  -- ^ Percent (<= 1.0) of `Kanji` that an adult should be able to read.
                         , density       :: Maybe Float  -- ^ Fraction (<= 1.0) of the input that was `Kanji`.
                         , distributions :: [(Level, Float)]  -- ^ Fractions of input `Kanji` that belong to each `Level`.
                         } deriving (Generic, ToJSON)

analysis :: Text -> Analysis
analysis t | T.null t = Analysis [] 0 0 0 0 Nothing []
           | otherwise = Analysis uniq (elementaryDen dist) (middleDen dist) (highDen dist) (adultDen dist) den (M.toList dist)
  where ks   = mapMaybe kanji $ unpack t
        uniq = M.toList . fmap S.toList $ uniques ks
        dist = levelDist ks
        den  = Just $ kanjiDensity (T.length t) ks
