{- © Tom Westbury (cathxrsis) tomwestbury1@gmail.com
 - HundredWeight.Printer => A set of printers for change ringing place notation
 - This module provides
 -
 -}

 module HundredWeight.Printer
 (
 ,
 ) where

-- A bell is represented as its symbol
charBell :: Bell -> Char
charBell (Bell _ b) =  intToDigit

-- Print a row to a string
printRow :: Row -> String
printRow r = ((map charBell) $ sort r) ++ ['\n']

-- Print each bell of the method
printMethod :: [Row] -> String
printMethod m = foldl (++) [] (map printRow m)
