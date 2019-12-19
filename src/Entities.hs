module Entities where 

import Data.Bool

data Message = NULL | Message {
    coord :: Cell,
    result :: String,
    next :: Message
} deriving Show

type Cell = (String, String)
data Scenario = PlayerA | PlayerB