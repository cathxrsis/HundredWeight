import Data.Char
import Data.List

-- Bell represents a bell's position and its number. Ordered by first number.
data Bell = Bell {pos :: Int, sym :: Int} deriving (Ord, Show)
-- The == operator is used to show when a bell is clashing with another.
instance Eq Bell where
   Bell a _ == Bell b _ = a == b

-- row is a row on a blue line, as a list of bell
type Row = [Bell]

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
   | otherwise          = minAbs xs

-- evolve takes in the stage the parsed place notation and a bell and produces Just a bell or Nothing.
evolve :: Int -> String -> Bell -> Bell
evolve _ [] (Bell p q)                               = Bell p q
evolve n (['x']) (Bell p q)
   | odd p                                           = Bell (p+1) q
   | otherwise                                       = Bell (p-1) q
evolve _ c (Bell p q)
   | (foldl (||) False (map isNumber c)) && (foldl (||) False (map (\r -> p == (digitToInt r)) c))   = Bell p q -- If the bell is one of the places, keep its position the same.
 --map calculate c-p. foldl take the smallest modulus value. if positive and even go down. if negative and odd go down. else go up
   | (even (minAbs (map ((\r -> r-p).digitToInt) c))) && ((minAbs (map ((\r -> r-p).digitToInt) c)) > 0) || (odd (minAbs (map ((\r -> r-p).digitToInt) c))) && ((minAbs (map ((\r -> r-p).digitToInt) c)) < 0) = Bell (p+1) q
   | (odd (minAbs (map ((\r -> r-p).digitToInt) c))) && ((minAbs (map ((\r -> r-p).digitToInt) c)) > 0) || (even (minAbs (map ((\r -> r-p).digitToInt) c))) && ((minAbs (map ((\r -> r-p).digitToInt) c)) < 0) = Bell (p-1) q
   | otherwise                                       = Bell p q --Left "Uncaught Place Notation Error"

--Requires cases for:
-- When in stage n and bell (n-odd) makes places, bell n must make places
-- When bell (1 + odd) makes places then 1 must make places (but not if there is an (1+even)<(1+odd))

--parsePlace :: Int -> String -> [String]

-- Generate a list of bells from a row
change :: Int -> String -> Row -> Row
change n s r = map (evolve n s) r

-- Generate non rounds method
methodNon :: Int -> [String] -> Row -> [Row]
methodNon n [] r = []
methodNon n (p:ps) r = (change n p r) : methodNon n ps (change n p r)

-- Start from rounds
method :: Int -> [String] -> [Row]
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

-- 
charBell :: Bell -> Char
charBell (Bell _ b) =  intToDigit b

-- Print a row to a string
printRow :: Row -> String
printRow r = ((map charBell) $ sort r) ++ ['\n']

printMethod :: [Row] -> String
printMethod m = foldl (++) [] (map printRow m)

--Features to do
-- Parser needs to parse changes into full notation
-- GUI
-- Check and highlight falseness (give row and highlight)
-- Allow for composing blocks of place notation
-- Allow for bob, single, plain and more exciting blocks
-- Allow for block composition

