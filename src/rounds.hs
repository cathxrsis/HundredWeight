import Data.Char
import Data.List
import Parsec

-- Bell represents a bell's position and its number. Ordered by first number.
data Bell = Bell {pos :: Int, sym :: Int} deriving (Ord, Show)
-- The == operator is used to show when a bell is clashing with another.
instance Eq Bell where
   Bell a _ == Bell b _ = a == b

-- row is a row on a blue line, as a list of bells
type Row = [Bell]
-- Places is a list of strings that each represent the notation for a change
type Places = [String]

-- rounds constructs a row of bells in the same position as their number
rounds :: Int -> Row
rounds 0 = []
rounds x = [Bell x x] ++ rounds (x-1)

-- finds the minimum absolute value from a list of integers (will pick first if two of equivalent abs value)
minAbs :: [Int] -> Int
minAbs (x:[]) = x
minAbs (x:y:xs)
   | (abs x) <= (abs y) = x
   | (abs y) <= (abs x) = y
   | otherwise = minAbs xs

-- evolve takes in the stage the parsed place notation and a bell and produces Just a bell or Nothing.
evolve :: Int -> String -> Bell -> Bell
evolve _ [] (Bell p q) = Bell p q
evolve n (['x']) (Bell p q)
   | odd p = Bell (p+1) q
   | otherwise = Bell (p-1) q
evolve _ c (Bell p q)
   | (foldOr $ map isNumber c) && (foldOr $ map sameP c) = Bell p q -- If the bell is one of the places, keep its position the same.
 --map calculate c-p. foldl take the smallest modulus value. if positive and even go down. if negative and odd go down. else go up
   | ((even $ cP c) && ((cP c) > 0)) || ((odd $ cP c) && ((cP c) < 0)) = Bell (p+1) q
   | ((odd $ cP c) && ((cP c) > 0)) || ((even $ cP c) && ((cP c) < 0)) = Bell (p-1) q
   | otherwise                                                         = Bell p q
   where cP = minAbs.(map ((\r -> r-p).digitToInt)) --Find the closest bell making a place to our bell
         foldOr = foldl (||) False
         sameP = (\r -> p == (digitToInt r))
--Requires cases for:
-- When in stage n and bell (n-odd.closestPlace $ c) makes places, bell n must make places
-- When bell (1 + odd.closestPlace $ c) makes places then 1 must make places (but not if there is an (1+even)<(1+odd))

-- Generate a list of bells from a row
change :: Int -> String -> Row -> Row
change n s r = map (evolve n s) r

-- Generate non rounds method
methodNon :: Int -> Places -> Row -> [Row]
methodNon n [] r = []
methodNon n (p:ps) r = (change n p r) : methodNon n ps (change n p r)

-- Start from rounds
method :: Int -> Places -> [Row]
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
charBell (Bell _ b) =  intToDigit b

-- Print a row to a string
printRow :: Row -> String
printRow r = ((map charBell) $ sort r) ++ ['\n']

-- Print each bell of the method
printMethod :: [Row] -> String
printMethod m = foldl (++) [] (map printRow m)



--parsePlace :: String -> [String]
--parsePlace []       = []
--parsePlace ('.':ss) = [] : (parsePlace ss)
--parsePlace ('x':ss) =
--parsePlace ('X':ss) =
--parsePlace ('-':ss) =
-- Work out if this is valid place notation
--isPlaceNotation :: Char -> Bool
--isPlaceNotation char = any (char ==) "1234567890xX-.,"

--placeNotation = ReadP [String]
--placeNotation = do

-- Parser Requirements:
-- pNParse :: Int -> String -> [String]
--  on numbers:
--    number <= stage = add to string
--    otherwise fail
--  on 'x/X':
--    end string and add "x" to listP
--  on '.':
--    end string, begin new one in listP
--  on ',':
--    end string, ++ reverse of listP onto listP then begin new on in listP
--  on [] :
--    end parse


--Features to do
-- Parser needs to parse changes into full notation
-- GUI
-- Check and highlight falseness (give row and highlight)
-- Allow for composing blocks of place notation
-- Allow for bob, single, plain and more exciting blocks
-- Allow for block composition
