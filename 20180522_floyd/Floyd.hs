{-# OPTIONS_GHC -Wall #-}

import Data.List as L (unfoldr)

floyd0 :: [[Int]]
floyd0 = loop 1 [1..] where
  loop n input =
    let header = take n input
        footer = loop (succ n) (drop n input)
    in  header : footer

floyd1 :: [[Int]]
floyd1 = L.unfoldr alg (1, 0) where
  alg (m, n) =
    let lbound = m
        ubound = lbound + n
        range  = [lbound..ubound]
    in  Just (range, (succ ubound, succ n))

render :: Show a => [a] -> String
render input = case input of
  []    -> ""
  [h]   -> show h
  (h:t) -> show h ++ " " ++ render t

main :: IO ()
main = do
  mapM_ (putStrLn . render) (take 5 floyd0)

  putStrLn ""

  mapM_ (putStrLn . render) (take 5 floyd1)

