module Sgf (parseSgf) where

import Control.Monad (liftM2)
--import Control.Monad.Combinators (some)
import Data.Char (isAlphaNum, isAlpha, isUpper, isSpace)
import Data.Map  (Map, fromList)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..))
import Data.Either.Combinators (rightToMaybe)
import Text.Parsec (Parsec, parse, between, many, many1, oneOf, satisfy, (<|>))
import Text.Parsec.Char (char, string, anyChar)
import Text.Parsec.Token (symbol)
-- import Text.Parsec.String (Parser)

type SgfNode = Map Text [Text]
type SgfTree = Tree SgfNode
type Parser  = Parsec Text ()

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

braket :: Parser a -> Parser a
braket = between (char '[') (char ']')

semicolon :: Parser String
semicolon = string ";"

keyText :: Parser String
keyText = many1 $ satisfy (liftM2 (&&) isAlpha isUpper)

alphaNum :: Parser String
alphaNum = many1 $ satisfy isAlphaNum

escape :: Parser Char
escape = (char '\\' >> anyChar) <|> (oneOf "\n\t" >> return ' ')

valueString :: Parser String
valueString = error "lol"

key :: Parser Text
key = pack <$> keyString

value :: Parser Text
value = pack <$> braket valueString

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