{-# OPTIONS_GHC -Wall #-}

import qualified Data.Set as S
import qualified Data.List as L

allUniqueFast :: String -> Bool
allUniqueFast = loop mempty where
  loop set input = case input of
    []           -> True
    (char:chars) ->
         not (S.member char set)
      && loop (S.insert char set) chars

allUniqueSlow :: String -> Bool
allUniqueSlow = loop Nothing . L.sort where
  loop acc input = case input of
    []           -> True
    (char:chars) -> case acc of
      Nothing   -> loop (Just char) chars
      Just prev ->
             (char /= prev)
          && loop (Just char) chars

pour :: [a] -> [a]
pour = loop mempty where
  loop acc input = case input of
    []    -> acc
    (h:t) -> loop (h:acc) t

