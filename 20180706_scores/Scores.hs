{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import qualified Control.Foldl as F
import qualified Data.Heap as H
import qualified Data.Map.Strict as MS

accrue :: (Ord k, Ord a, Num a) => F.Fold (k, a) (MS.Map k (H.Heap a))
accrue = F.Fold alg mempty id where
  alg !acc (name, score) =
    let updater = \case
          Nothing -> Just (H.singleton score)
          Just sc -> Just (
            if   H.size sc >= 5
            then case H.uncons sc of
                   Nothing     -> error "impossible"
                   Just (m, h) ->
                     if   m < score
                     then H.insert score h
                     else sc
            else H.insert score sc)

    in  MS.alter updater name acc

-- | Slightly different from spec: calculates average top scores, but using a
--   *maximum* of five scores (not at least five scores).
topFiveAvg
  :: (Foldable f, Ord k, Ord b, Fractional b)
  => f (k, b) -> MS.Map k b
topFiveAvg = fmap (F.fold F.mean) . F.fold accrue

test :: [(String, Double)]
test = [
    ("Jared", 72)
  , ("Jared", 71)
  , ("Jared", 81)
  , ("Jared", 65)
  , ("Jared", 51)
  , ("Jared", 62)
  , ("Jared", 78)
  , ("Jared", 14)
  , ("Shawn", 95)
  , ("Shawn", 98)
  , ("Shawn", 89)
  , ("Shawn", 81)
  , ("Shawn", 98)
  , ("Shawn", 91)
  , ("Shawn", 76)
  , ("Rachel", 99)
  , ("Rachel", 99)
  , ("Rachel", 100)
  , ("Rachel", 81)
  , ("Deanie", 99)
  , ("Deanie", 75)
  , ("Deanie", 76)
  , ("Deanie", 74)
  , ("Deanie", 43)
  , ("Deanie", 69)
  ]

main :: IO ()
main = print (topFiveAvg test)
