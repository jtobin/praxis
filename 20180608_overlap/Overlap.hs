{-# OPTIONS_GHC -Wall #-}

data Range = Range !Int !Int
  deriving (Eq, Show)

range :: Int -> Int -> Maybe Range
range a b
  | a <= b    = Just (Range a b)
  | otherwise = Nothing

overlap :: Range -> Range -> Maybe Range
overlap (Range al bl) (Range ar br)
  | bl < ar || br < al = Nothing
  | otherwise          = range (max al ar) (min bl br)

main :: IO ()
main = do
  let bar = do
        r0 <- range 17 25
        r1 <- range 12 19
        overlap r0 r1

      baz = do
        r0 <- range 12 17
        r1 <- range 19 25
        overlap r0 r1

      qux = do
        r0 <- range 19 25
        r1 <- range 12 17
        overlap r0 r1

      zap = do
        r0 <- range 19 25
        r1 <- range 22 30
        overlap r0 r1

  print bar
  print baz
  print qux
  print zap


