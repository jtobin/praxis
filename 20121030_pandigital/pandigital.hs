-- Spec: a 3-digit number is added to another 3-digit number, and the result is 
-- a 4-digit number.  If the ten digits involved are all different (0 thru 9)
-- then what is the smallest possible value for any of the three numbers?

import Data.List

-- nums = [(x, y, x + y) | x <- 

pandigitals = [(x, y, x + y) | x <- shared, y <- shared, x < y, x + y > 999, unique [x, y, x + y]]
     where shared = filter (unique . return) [100..999]
           unique = (\x -> x == nub x) . (show =<<)

unique :: Show a => [a] -> Bool
unique = (\x -> x == nub x) . (show =<<)

-- stringize :: Show a => [a] -> String
-- stringize = (show =<<)

