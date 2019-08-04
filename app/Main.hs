module Main where

import Lib
import System.Environment
import HundredWeight.Core

main :: IO ()
-- Takes in a place notation and prints the notations
main = do
  args <- getArgs -- getArgs :: IO [String]
