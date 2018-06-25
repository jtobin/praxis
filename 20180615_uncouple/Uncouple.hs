{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

import Data.Function (fix)

coupled :: Eq a => [a] -> Bool
coupled xs =
  let loop rec acc recent = \case
        []     -> acc
        (h:t) -> (Just h == recent) || rec False (Just h) t

  in  fix loop False Nothing xs

decouple :: Eq a => [a] -> [a]
decouple xs =
  let loop rec acc recent = \case
        []    -> acc
        (h:t) ->
          if   Just h == recent
          then rec (drop 1 acc) Nothing t
          else rec (h : acc) (Just h) t

  in  reverse (fix loop [] Nothing xs)

uncouple :: Eq a => [a] -> [a]
uncouple xs =
  let loop rec input
        | coupled input = rec (decouple input)
        | otherwise     = input

  in  fix loop xs

data Colour =
    Red
  | Blue
  | Green
  deriving (Eq, Show)

main :: IO ()
main = print (uncouple [Red, Red, Blue, Green, Green, Blue, Green])

