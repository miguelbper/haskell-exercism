module Sgf (parseSgf) where

import Data.Map  (Map)
import Data.Text (Text)
import Data.Tree (Tree)
import Text.Parsec (Parsec, parse)
import Data.Either.Combinators (rightToMaybe)


type SgfNode = Map Text [Text]
type SgfTree = Tree SgfNode

parseSgf :: Text -> Maybe SgfTree
parseSgf = rightToMaybe . parse parser ""

parser :: Parsec Text () SgfTree
parser = error "lol"
