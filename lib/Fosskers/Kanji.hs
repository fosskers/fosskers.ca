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
                         , average       :: Maybe Float  -- ^ Average `Level` of all `Kanji` present.
                         , density       :: Maybe Float  -- ^ Fraction (<= 1.0) of the input that was `Kanji`.
                         , unknowns      :: [Kanji]  -- ^ `Kanji` whose `Level` can't be determined.
                         , distributions :: [(Level, Float)]  -- ^ Fractions of input `Kanji` that belong to each `Level`.
                         } deriving (Generic, ToJSON)

analysis :: Text -> Analysis
analysis t | T.null t = Analysis [] 0 Nothing Nothing [] []
           | otherwise = Analysis uniq (elementaryDen dist) (averageLevel ks) den uns (M.toList dist)
  where ks   = mapMaybe kanji $ unpack t
        uniq = M.toList . fmap S.toList $ uniques ks
        dist = levelDist ks
        uns  = S.toList . S.filter (isNothing . level) $ S.fromList ks
        den  = Just $ kanjiDensity (T.length t) ks
