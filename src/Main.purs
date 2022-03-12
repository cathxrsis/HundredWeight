module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List
import Data.Tuple
import Text.Parsing.Parser
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Combinators
import Data.Lens
import Data.Lens.Record (prop)
import Data.Symbol

main :: Effect Unit
main = do
  log "🍝"

-- Internal Parse Tree for Place Notation

type PNParseTree = {stageT :: Int, notT :: Tuple (Array  (Array Int)) (Array Int)}

_stageT :: forall a r. Lens' {stageT :: a | r} a
_stageT = prop (SProxy :: SProxy "stageT")

_notT :: forall a r. Lens' {notT :: a | r} a
_notT = prop (SProxy :: SProxy "notT")

emptyPNPT = {stageT: 0, notT: Tuple [[]] []}

-- Type for extended parse tree
type PNPlaces = {stageP :: Int, notP :: Array (Array Int)}

_stageP :: forall a r. Lens' {stageP :: a | r} a
_stageP = prop (SProxy :: SProxy "stageP")

_notP :: forall a r. Lens' {notP :: a | r } a
_notP = prop (SProxy :: SProxy "notP")

pPBD :: PNPlaces
pPBD =
  {stageP: 5
  , notP: [[0x5], [0x1], [0x5], [0x1], [0x5], [0x1], [0x5], [0x1], [0x5], [0x1, 0x2]]
  }

-- Convert PNParseTree to PNPlaces for creation of numbers or BlueLines
--expand :: PNParseTree -> PNParser
--expand = ?hole

type PNParser = Parser String PNParseTree

