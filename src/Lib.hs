 import Data.Set (Set)
 import qualified Data.Set as Set

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Possible symbols for bells from 1 to 16 (D)
-- data BellSymbol = '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0'|'E'|'T'|'A'|'B'|'C'|'D' deriving (Eq, Ord, Show)

-- Bell represents a single bell and its position in the current change
data Bell = Bell {pos : Int, sym : Int} deriving (Eq, Ord, Show)

-- Constructs a change of bells in rounds
Rounds :: (Int a) => a -> [Bell]
Rounds x = do
	i <- [1 .. x]
	return (bell i i)

-- Define a Row as a list of bells
type Row = [Bell]

ParsePlace :: (String a) => a -> Either [a]
-- Turns place notation into an array of changes
ParsePlace (c : '.' : cs) = c : ParsePlace cs
Parseplace (c : 'x' : cs) = c : 'x' : ParsePlace cs

-- Takes a bit of place notation and a row and returns a 
Change :: (String a, Row b) => a -> b -> b
Change "x" r = 
Change ('1':xs) ()

-- Define a Method as a list of Rows
type Method = [Row]

-- Takes place notation and turns it into a method
constructMethod :: (String a, Int b, Method c) => a -> b -> c
constructMethod = do
	i <- ParsePlace a
	j <- Change i Rounds $ b 

