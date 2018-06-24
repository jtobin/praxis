{-# OPTIONS_GHC -Wall #-}

import Data.Bits ((.&.))
import Data.Function (fix)

hweight :: Int -> Int
hweight m =
  let loop r acc j
        | j == 0    = acc
        | otherwise = r (succ acc) (j .&. pred j)

  in  fix loop 0 m

next :: Int -> Int
next m =
  let hw = hweight m

      loop r n
        | hweight n == hw = n
        | otherwise       = r (succ n)

  in  fix loop (succ m)

main :: IO ()
main = print (next 7)

