import System.Random.MWC
import Control.Monad

data Choice = Switch | Stay deriving Eq

montyRandSwitch g = do
    z0     <- uniformR (0, 2) g        :: IO Int                    
    switch <- uniformR (False, True) g :: IO Bool
    return $ if switch 
             then (z0 /= 2, Switch)
             else (z0 == 2, Stay)

runAltSimulation n g = do
     ws <- liftM (filter fst) (replicateM n (montyRandSwitch g))
     let winBySwitch = length . filter ((== Switch) . snd) $ ws
         winByStay   = length . filter ((== Stay)   . snd) $ ws
     return (winBySwitch, winByStay)

main = do 
    g <- create 
    replicateM 10 (runAltSimulation 10000 g)

