{- © Tom Westbury (cathxrsis) tomwestbury1@gmail.com
 - HundredWeight.Parser => A parser for change ringing place notation
 - This module provides
 -
 -}


module HundredWeight.Parser
(
,
) where

import HundredWeight.Core
import Data.Char
import Data.List
import Control.Applicative
import Text.Parsec
import Text.ParserCombinators.Parsec

-- Handy shorthand
(<||>) :: (Applicative a) => a -> a -> a
(<||>) x y = (try x) <|> y

-- PlaceNotTok represents the Toks for place notation parsing
data PlaceNotTok = Xtok | Palindrome | Place Int | Dot deriving Show

-- Overall parser function
parsePlaceNotation :: String -> [PlaceNotation]
parsePlaceNotation = parsePlace.retParsePlace.chompLex.retChomp

-- Take a list and return something that can be chomped into another
retChomp :: [a] -> ([PlaceNotTok], [a])
retChomp s = ([], s)

-- Chomp up a list of ss and transform into as ignoring Nothings
chomp :: (s -> Maybe a) -> ([a], [s]) -> [a]
chomp _ (ps ,[]) = ps
chomp f (ps, (c:cs)) = chomp f ((f c) ++ ps, cs)

-- Create a lexer chomper
chompLex = chomp lexPlace

-- Create tokens for all of the lexy things but ignore errors
lexPlace :: Char -> [PlaceNotTok]
-- all change tokens
lexPlace 'x' = Xtok : []
lexPlace 'X' = Xtok : []
lexPlace '-' = Xtok : []
-- Palindrome token
lexPlace ',' = Palindrome : []
-- call tokens
lexPlace '.' = [Dot]
lexPlace '0' = [Place 10]
lexPlace 'E' = [Place 11]
lexPlace 'e' = [Place 11]
lexPlace 'T' = [Place 12]
lexPlace 't' = [Place 12]
lexPlace p
  | isNumber p = [Place $ digitToInt p]
  | otherwise = []
-- Must cope with numbers above 12, test for alphabet and to upper it

-- Helper function to turn tokens into place notation
toPn :: PlaceNotTok -> parsePlace (pns, (p:ps)) = parsePlace (((toPn p):pns), ps)
PlaceNotation
toPn Xtok = X
toPn (Place p) = Change [p]
toPn Dot = Change []

-- lifts a list of place not tokens into a parse ready type
retParsePlace :: [PlaceNotTok] -> ([PlaceNotation],[PlaceNotTok])
retParsePlace p = ([], (reverse p))

-- Parse a list of lexed tokens
parsePlace :: ([PlaceNotation], [PlaceNotTok]) -> [PlaceNotation]
parsePlace (pns, []) = pns
parsePlace (((Change []) : pns), ((Place x) : ps)) = parsePlace (((Change [x]) : pns),ps)
parsePlace (((Change cs):pns), (Place c):ps) = parsePlace (((Change (c:cs)):pns), ps)
parsePlace ((pn:pns), (Palindrome : ps)) = parsePlace (((reverse pns) ++ [pn] ++ pns), ps)
parsePlace (pns, (p:ps)) = parsePlace (((toPn p):pns), ps)

-- Parser tree for bellringing place Notation
