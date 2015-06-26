
module Replace where

stringReplace :: String -> String -> String -> String
stringReplace pat rep = foldr alg [] where
  patLength = length pat
  alg c acc
    | take patLength (c:acc) == pat = rep ++ drop patLength (c:acc)
    | otherwise                     = c : acc

