module Main where

import System.Environment
import Game

main :: IO ()
main = do
    args <- getArgs
    gameLoop (args !! 0) (args !! 1)