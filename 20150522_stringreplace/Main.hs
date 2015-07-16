
module Main where

import System.Environment

data Replace a = Replace {
    pattern     :: [a]
  , replacement :: [a]
  }

stringReplace :: Eq a => [a] -> [a] -> [a] -> [a]
stringReplace pat rep = foldr alg [] where
  patLength = length pat
  alg c acc
    | take patLength (c:acc) == pat = rep ++ drop patLength (c:acc)
    | otherwise                     = c : acc

replace :: Eq a => Replace a -> [a] -> [a]
replace (Replace pat rep) = foldr alg [] where
  patLength = length pat
  alg c acc
    | take patLength (c:acc) == pat = rep ++ drop patLength (c:acc)
    | otherwise = c : acc

main :: IO ()
main = do
  args <- getArgs
  case args of
    (pat:rep:str:_) -> putStrLn (stringReplace pat rep str)
    _ -> putStrLn "USAGE: ./stringReplace PATTERN REPLACEMENT STRING"

