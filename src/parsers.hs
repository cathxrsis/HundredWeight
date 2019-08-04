{- © Tom Westbury (cathxrsis) tomwestbury1@gmail.com
 - HundredWeight.Parsers => A set of parsers for change ringing place notation
 - This module provides
 -
 -}

 module HundredWeight.Parser
 (
 ,
 ) where

import HundredWeight.Core
import Control.Applicative
import Text.Parsec
import Text.ParserCombinators.Parsec

-- Parse Place Notation
type PNParser :: GenParser Char st [String]

-- Handy shorthand for chompy parsers
infixr 5 <||>
(<||>) :: (Applicative a) => a -> a -> a
(<||>) x y = (try x) <|> y

placeNotationParser :: PNParser
placeNotationParser = do
  places <- palindromeParser <||> placesParser
  return places

palindromeParser :: PNParser
palindromeParser = do
  palindrome <- many placeParse
  leadEnd <- placeParser palindrome
  return leadEnd

placesParser = do
  placeParser

placeParser =
  (char '.' *> placesParser)
  <|> (char 'X' *> placesParser)

-- Parse Args to allow for pretty printing of output &c.
