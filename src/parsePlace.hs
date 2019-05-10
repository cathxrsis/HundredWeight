import Data.Char
import Data.List

-- PlaceNotation represents the parse tree for place notation
data PlaceNotation = X | Change [Int] deriving Show

-- PlaceNotTok represents the Toks for place notation parsing
data PlaceNotTok = Xtok | Palindrome | Place Int | Dot deriving Show

catIf :: (Maybe a) -> [a] -> [a]
--
catIf Nothing ps = ps
catIf (Just p) ps = ps ++ [p]

retChomp :: [a] -> ([PlaceNotTok], [a])
-- Take a list and return something that can be chomped into another
retChomp s = ([], s)

chomp :: (s -> Maybe a) -> ([a], [s]) -> [a]
-- Chomp up a list of ss and transform into as ignoring Nothings
chomp _ (ps ,[]) = ps
chomp f (ps, (c:cs)) = chomp f (catIf (f c) ps, cs)

lexPlace :: Char -> Maybe PlaceNotTok
-- all change tokens 
lexPlace 'x' = Just Xtok
lexPlace 'X' = Just Xtok
lexPlace '-' = Just Xtok
-- Palindrome token
lexPlace ',' = Just Palindrome
-- call tokens
lexPlace '.' = Just Dot
lexPlace '0' = Just $ Place 10
lexPlace 'E' = Just $ Place 11
lexPlace 'e' = Just $ Place 11
lexPlace 'T' = Just $ Place 12
lexPlace 't' = Just $ Place 12
lexPlace p
  | isNumber p = Just $ Place $ digitToInt p
  | otherwise = Nothing
-- Must cope with numbers above 12, test for alphabet and to upper it

toPn :: PlaceNotTok -> PlaceNotation
toPn Xtok = X
toPn Place p = Change [p]

parsePlace :: ([PlaceNotation], [PlaceNotTok]) -> [PlaceNotation]
parsePlace (pns, []) = reverse pns
parsePlace (((Change cs):pns), (Place c):ps) = parsePlace (((Change (c:cs)):pns), ps)
parsePlace ((pn:pns), (Palindrome : ps)) = (((reverse pns) ++ pn ++ pns), ps)
parsePlace (pns, (p:ps)) = parsePlace (((toPN p):pns), ps)
-- Needs to cope with dots

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
