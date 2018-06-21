{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as MS
import Data.Monoid ((<>), mconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let parsed = case args of
        (input : _) -> parse (T.pack input)
        _           -> Nothing

  case parsed of
    Nothing -> do
      TIO.putStrLn "usage: ./clock HH:MM"
      TIO.putStrLn "(where HH in 00 -- 23, MM in 00 -- 59)"

    Just time ->
      TIO.putStrLn time

parse :: T.Text -> Maybe T.Text
parse input = case T.splitOn ":" stripped of
    [hrs, mins] -> do
      (rhrs, am) <- MS.lookup hrs hourmap
      rmin       <- MS.lookup mins minutemap
      return (present (rhrs, rmin, am))

    _ -> Nothing

  where
    stripped = T.strip input

hourmap :: MS.Map T.Text (T.Text, Bool)
hourmap = MS.fromList (zip hours rhours)

minutemap :: MS.Map T.Text T.Text
minutemap = MS.fromList (zip minutes rminutes)

present :: (T.Text, T.Text, Bool) -> T.Text
present (h, m, am) = "It's " <> h <> " " <> mins <> meridiem where
  mins
    | m == T.empty = m
    | otherwise    = m <> " "
  meridiem
    | am        = "am"
    | otherwise = "pm"

onesAndTens :: [T.Text]
onesAndTens = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
  , "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen" , "sixteen"
  , "seventeen", "eightteen", "nineteen"
  ]

gentext :: T.Text -> T.Text -> [T.Text]
gentext leader prefix = mconcat [
    [leader]
  , fmap ((prefix <> " ") <>) (take 9 onesAndTens)
  ]

hours :: [T.Text]
hours =
  [ "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10" , "11"
  , "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22" , "23"
  ]

rhours :: [(T.Text, Bool)]
rhours = mconcat
    [ ("twelve", True)  : zip hrs (repeat True)
    , ("twelve", False) : zip hrs (repeat False)
    ]
  where
    hrs = take 11 onesAndTens

minutes :: [T.Text]
minutes = mconcat [
      fmap ("0" <>) digs
    , fmap ("1" <>) digs
    , fmap ("2" <>) digs
    , fmap ("3" <>) digs
    , fmap ("4" <>) digs
    , fmap ("5" <>) digs
    ]
  where
    digs = fmap render ([0..9] :: [Int])

    render :: Show a => a -> T.Text
    render = T.pack . show

rminutes :: [T.Text]
rminutes = mconcat [
    gentext "" "oh"
  , drop (9 :: Int) onesAndTens
  , gentext "twenty" "twenty"
  , gentext "thirty" "thirty"
  , gentext "forty" "forty"
  , gentext "fifty" "fifty"
  ]

