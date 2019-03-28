import Data.Char

-- Bell represents a bell's position and its number. Ordered by first number.
data Bell = Bell {pos :: Int, sym :: Int} deriving (Ord, Show)
-- The == operator is used to show when a bell is clashing with another.
instance Eq Bell where
   Bell a _ == Bell b _ = a == b


-- row is a row on a blue line, as a list of bells
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
   | otherwise          = minAbs xs
 
-- changes takes in the stage the parsed place notation and a bell and produces Either a bell or an error.
change :: Int -> String -> Bell -> Maybe Bell
change n (['x']) (Bell p q)
   | odd n                                           = Nothing --Left "Use of x in odd method stage"
   | odd p                                           = Just $ Bell (p+1) q
   | otherwise                                       = Just $ Bell (p-1) q
change _ c (Bell p q)
   | (foldl (||) (map isNumber c)) && (foldl (||) (map (\r -> p == (digitToInt r)) c))   = Just $ Bell p q -- If the bell is one of the places, keep its position the same.
 --map calculate c-p. foldl take the smallest modulus value. if positive and even go down. if negative and odd go down. else go up
   | (even (minAbs (map ((-p).digitToInt) c))) && ((minAbs (map ((-p).digitToInt) c)) > 0) || (odd (minAbs (map (-p) c))) && ((minAbs (map ((-p).digitToInt) c)) < 0) = Just $ Bell (p-1) q
   | (odd (minAbs (map ((-p).digitToInt) c))) && ((minAbs (map ((-p).digitToInt) c)) > 0) || (even (minAbs (map (-p) c))) && ((minAbs (map ((-p).digitToInt) c)) < 0) = Just $ Bell (p+1) q
   | otherwise                                       = Nothing --Left "Uncaught Place Notation Error"
 
 
--Features
-- Parser needs to parse changes into full notation
-- Check and highlight falseness (give row and highlight)
-- Allow for composing blocks of place notation
-- Allow for bob, single, plain and more exciting blocks
-- Allow for block composition
 
-- digitToInt



