{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (compare)

len :: [a] -> Int
len = loop 0 where
  loop !acc = \case
    []    -> acc
    (_:t) -> loop (succ acc) t

compare :: [a] -> [b] -> Ordering
compare = loop where
  loop l r = case (l, r) of
    (_ : _, []) -> GT
    ([], _ : _) -> LT
    ([], [])    -> EQ
    _           -> loop (drop 1 l) (drop 1 r)

main :: IO ()
main = do
  print (len [1..1000])
  print (compare [1..10] [1..])
