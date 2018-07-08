{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Control.Foldl as L
import Data.Char as C
import qualified Data.Foldable as FO
import qualified Data.Map.Strict as MS
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Prelude.Text as PT
import System.IO

data Record = Record {
    rnum :: !Int
  , rcrs :: !T.Text
  , rgra :: !Int
  } deriving (Eq, Show)

parse :: T.Text -> Record
parse (T.strip -> line) =
  case T.splitOn "|" line of
    [tnum, rcrs, tgra] ->
      let rnum = parseNum tnum
          rgra = parseNum tgra

      in  Record {..}

    _ -> error "parse: bad input"

parseNum :: T.Text -> Int
parseNum =
    snd . T.foldr alg (1, 0)
  where
    alg char (base, acc) =
      let nbase = base * 10
          nacc  = acc + C.digitToInt char * base
      in  (nbase, nacc)

collect :: L.Fold Record (MS.Map T.Text Record)
collect = L.Fold alg mempty id where
  alg acc rnew@(Record nnew cnew _) =
    case MS.lookup cnew acc of
      Nothing          -> MS.insert cnew rnew acc
      Just Record {..} ->
        if   nnew < rnum
        then MS.insert rcrs rnew acc
        else acc

render :: Record -> T.Text
render Record {..} = rcrs <> ": " <> (T.pack . show) rgra

main :: IO ()
main = do
  file <- openFile "grades.dat" ReadMode

  let handle = PT.fromHandleLn file
      pipe   = P.for handle (P.yield . parse)

  result <- L.purely P.fold collect pipe

  mapM_ (T.putStrLn . render) (FO.toList result)

