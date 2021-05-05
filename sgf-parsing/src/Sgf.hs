module Sgf (parseSgf) where

import Control.Monad (liftM2)
import Data.Char (isAlphaNum, isSpace)
import Data.Map  (Map, fromList)
import Data.Text (Text, pack)
import Data.Tree (Tree(..))
import Data.Either.Combinators (rightToMaybe)
import Text.Parsec (Parsec, parse, between, many, many1, oneOf, satisfy, (<|>))
import Text.Parsec.Char (char, string, anyChar, upper)

type SgfNode = Map Text [Text]
type SgfTree = Tree SgfNode
type Parser  = Parsec Text ()

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

braket :: Parser a -> Parser a
braket = between (char '[') (char ']')

semicolon :: Parser String
semicolon = string ";"

valueChar :: Parser Char
valueChar = (char '\\' >> anyChar) 
        <|> (oneOf "\n\t" >> return ' ') 
        <|> satisfy (liftM2 (||) isAlphaNum isSpace)

value :: Parser Text
value = pack . filter (`notElem` "\n\t") <$> braket (many1 valueChar)

key :: Parser Text
key = pack <$> many1 upper

values :: Parser [Text]
values = many1 value

property :: Parser (Text, [Text])
property = liftM2 (curry id) key values

properties :: Parser [(Text, [Text])]
properties = many1 property

node :: Parser SgfNode
node = fromList <$> properties

nodes :: Parser SgfTree
nodes = semicolon >> 
        liftM2 Node (node <|> return mempty) 
                    (((:[]) <$> nodes) <|> many tree <|> return []) 

tree :: Parser SgfTree
tree = parens nodes

parseSgf :: Text -> Maybe SgfTree
parseSgf = rightToMaybe . parse tree ""