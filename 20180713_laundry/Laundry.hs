{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

test :: String
test =
  mconcat [
      "ABCDE This is some text.", "\n"
    , "This is more text. ABCDE, ABCDE.", "\n"
    , "ABCDE And this is [ABCDE] still more text."
    ]

fnr :: String -> String
fnr = loop 1 mempty where
  loop j acc input = case input of
    []      -> reverse acc
    ('A':t) ->
      if    match input
      then
        let label = show j ++ "X"
        in  loop (succ j) (label ++ acc) (drop 5 input)
      else
        loop j ('A':acc) t

    (h:t)   -> loop j (h:acc) t

match :: String -> Bool
match input = take 5 input == "ABCDE"
