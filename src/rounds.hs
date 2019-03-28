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


minMag :: [Int] -> Int
minMag x:xs
   | (abs x)
minMag x:xs
   | (abs x) > (minMag xs) = minMag xs
 
-- changes takes in the stage the parsed place notation and a bell and produces Either a bell or an error.
change :: Int -> String -> Bell -> Either Bell
change n ('x') (p,q)
   | odd n                                           = Left "Use of x in odd method stage"
   | odd p                                           = Right ((p+1),q)
   | otherwise                                       = Right ((p-1),q)
change _ c (p,q)
   | (foldl (||) (map isNumber c)) && (foldl (||) (map (\r -> p == (digitToInt r)) c))   = Right (p,q) -- If the bell is one of the places, keep its position the same.
   | (foldl (||) (map isNumber c)) && (foldl (||) (map (odd.digitToInt) c)) && (odd p)   = Right (p-1,q) -- doesn't work for now as more than on place
-- map calculate c-p. foldl take the smallest modulus value. if positive and even go down. if negative and odd go down. else go up.
   | even (minMag (map (-p) c))
   | otherwise                                       = Left "Uncaught Place Notation Error"
 
 
--Features
-- Parser needs to parse changes into full notation
-- Check and highlight falseness (give row and highlight)
-- Allow for composing blocks of place notation
-- Allow for bob, single, plain and more exciting blocks
-- Allow for block composition
 
-- digitToInt

