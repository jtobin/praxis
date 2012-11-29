import System.Random.MWC
import Control.Monad

data Choice = Switch | Stay deriving Eq

monty switch g = do 
    z0 <- uniformR (0, 2) g :: IO Int
    return $ if switch then z0 /= 2 else z0 == 2

montyRandSwitch g = do
    z0 <- uniformR (0, 2) g :: IO Int
    zc <- uniformR (0, 1) g :: IO Double
    let switch = zc < 0.5
    return $ if switch then (z0 /= 2, Switch) else (z0 == 2, Stay)

runAltSimulation n g = do
     rs <- liftM (filter fst) (replicateM n (montyRandSwitch g))
     let winBySwitch = length . filter ((== Switch) . snd) $ rs
         winByStay   = length . filter ((== Stay)   . snd) $ rs
     return (winBySwitch, winByStay)

runSimulation n g = do 
    [n0, n1] <- mapM runMontyOnSwitch [True, False]
    putStrLn $ "proportion of wins, switching: " ++ show (n0 / fromIntegral n)
    putStrLn $ "proportion of wins, staying:   " ++ show (n1 / fromIntegral n)
  where runMontyOnSwitch b = liftM (sum . map (\a -> if a then 1 else 0)) 
                               (replicateM n (monty b g)) 

main = create >>= runSimulation 10000

