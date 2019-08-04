{- © Tom Westbury (Cathxrsis) tomwestbury1@gmail.com
 - Changes.Eval => An Evaluator for change ringing
 - This module provides
 -
 -}

import Data.Char
import Data.List
-- import Control.Applicative

-- PlaceNotation represents the parse tree for place notation
data Places = X | Change [Int] deriving Show

-- Bell represents a bell's position and its number. Ordered by first number.
data Bell = Bell {pos :: Int, sym :: Int} deriving (Ord, Show)
-- The == operator is used to show when a bell is clashing with another.
instance Eq Bell where
   Bell a _ == Bell b _ = a == b

-- row is a row on a blue line, as a list of bells
type Row = [Bell]

changeFromChar :: [Char] -> Places
-- Dangerous fn for testing, will be replaced by a safe Parser
changeFromChar [c] = Change [digitToInt c]

-- rounds constructs a row of bells in the same position as their number
rounds :: Int -> Row
rounds 0 = []
rounds x = [Bell x x] ++ rounds (x-1)

-- finds the minimum absolute value from a list of integers (will pick first if two of equivalent abs value)
minAbs :: Int -> Int -> Int
minAbs x y
   | (abs x) <= (abs y) = x
   | otherwise = y

-- evolve takes in the stage the parsed place notation and a bell and produces Just a bell or Nothing.
evolve :: Places -> Bell -> Bell
evolve X (Bell p q)
   | odd p =  Bell (p+1) q
   | otherwise = Bell (p-1) q
evolve (Change cs) (Bell p q)
   | (foldOr $ map (\r -> r == p) cs) = Bell p q -- If the bell is one of the places, don't swap
   | ((even $ cP cs) && ((cP cs) > 0)) || ((odd $ cP cs) && ((cP cs) < 0)) = Bell (p+1) q
   | ((odd $ cP cs) && ((cP cs) > 0)) || ((even $ cP cs) && ((cP cs) < 0)) = Bell (p-1) q
   | otherwise = Bell p q
   where cP = foldMinAbs.(map (\r -> r-p)) --Find the closest bell making a place to our bell
         foldOr = foldl (||) False
         foldMinAbs = foldl (minAbs) 36

-- Generate a list of bells from a row
change :: Places -> Row -> Row
change s r = map (evolve s) r

-- Generate non rounds method
methodNon :: Int -> [Places] -> Row -> [Row]
methodNon n [] r = []
methodNon n (p:ps) r = (change p r) : methodNon n ps (change p r)

-- Start from rounds
method :: Int -> [Places] -> [Row]
method a b = (methodNon a b) $ (rounds a)

stage :: Int -> String
stage 2 = "Micromus"
stage 3 = "Singles"
stage 4 = "Minimus"
stage 5 = "Doubles"
stage 6 = "Minor"
stage 7 = "Triples"
stage 8 = "Major"
stage 9 = "Caters"
stage 10 = "Royal"
stage 11 = "Cinques"
stage 12 = "Maximus"
stage 13 = "Sextuples"
stage x = "Stage " ++ show x

-- A bell is represented as its symbol
charBell :: Bell -> Char
charBell (Bell _ b) = intToDigit b

-- Print a row to a string
printRow :: Row -> String
printRow r = ((map charBell) $ sort r) ++ ['\n']

-- Print each bell of the method
printMethod :: [Row] -> String
printMethod m = foldl (++) [] (map printRow m)
