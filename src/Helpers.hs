module Helpers where

import Entities
import System.Random 

translateCoord :: (Int, Int) -> Cell
translateCoord (coordX, coordY) = 
    case coordX of
        1 -> ("A", show coordY)
        2 -> ("B", show coordY)
        3 -> ("C", show coordY)
        4 -> ("D", show coordY)
        5 -> ("E", show coordY)
        6 -> ("F", show coordY)
        7 -> ("G", show coordY)
        8 -> ("H", show coordY)
        9 -> ("I", show coordY)
        10 -> ("J", show coordY)

getRandomList :: Int -> IO([Int])
getRandomList 0 = return []
getRandomList n = do
    r  <- randomRIO (1,10)
    rs <- getRandomList (n-1)
    return (r:rs) 

getRandomCoord :: IO(Cell)
getRandomCoord = do
    randomCoord <- getRandomList 2
    let coord = (randomCoord !! 0, randomCoord !! 1)
    return $ translateCoord coord