{-# OPTIONS_GHC -Wall #-}

import Data.Char as C

esum :: String -> Int
esum = sum . grab

grab :: String -> [Int]
grab = loop [] Nothing where
  loop acc prev list = case list of
    []          -> reverse $ case prev of
      Nothing  -> acc
      Just num ->
        let parsed = parse (reverse num)
        in  parsed : acc

    (curr:rest) -> case prev of
      Nothing ->
        if   C.isDigit curr
        then loop acc (Just [curr]) rest
        else loop acc Nothing rest

      Just num ->
        if   C.isDigit curr
        then loop acc (Just (curr : num)) rest
        else
          let parsed = parse (reverse num)
          in  loop (parsed : acc) Nothing rest

parse :: String -> Int
parse =
    snd . foldr alg (1, 0)
  where
    alg char (base, acc) =
      let nbase = base * 10
          nacc  = acc + C.digitToInt char * base
      in  (nbase, nacc)

test0 :: String
test0 = "11aa22bb33cc44"

test1 :: String
test1 = "1k23jk34jk56jk3454"

main :: IO ()
main = do
  print (esum test0)
  print (esum test1)
