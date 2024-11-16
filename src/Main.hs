module Main where

import GHC.Base (List)
import Text.Parsec
import Text.Parsec.String (Parser)

type Code = [Statement]

type Var = String

data Statement = Print Var | Assign Var Value deriving (Eq, Show)

data Value = IntVal Int | StringVal String | BoolVal Bool | ListVal (List Value) deriving (Eq, Show)

printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)

intValParser :: Parser Value
intValParser = IntVal <$> (read <$> many1 digit)

-- TODO: This allows strings like "123foo", which aren't supported in Python
-- FIXME: This version doesn't work
stringValParser :: Parser Value
-- stringValParser = StringVal <$> ((char '"' <|> char '\'') >> many1 (noneOf "\"" <|> noneOf "\'") <* (char '"' <|> char '\''))
stringValParser = StringVal <$> ((char '"' <|> char '\'') >> many (noneOf "\"'") <* (char '"' <|> char '\''))

boolValParser :: Parser Value
boolValParser = BoolVal <$> ((True <$ string "True") <|> (False <$ string "False"))

listValParser :: Parser Value
listValParser = ListVal <$> (char '[' >> ((valueParser <* many space) `sepBy` (char ',' <* many space)) <* char ']')

valueParser :: Parser Value
valueParser = try intValParser <|> boolValParser <|> stringValParser <|> listValParser

assignParser :: Parser Statement
assignParser = Assign <$> (many1 letter) <*> (many1 space >> char '=' >> many1 space >> valueParser)

statementParser :: Parser Statement
statementParser = try printParser <|> try assignParser

codeParser :: Parser Code
codeParser = spaces >> many (statementParser <* many1 space) <* eof

type KeyValuePair = (String, Value)

keyValueParser :: Parser KeyValuePair
keyValueParser = (,) <$> (many1 letter <* many space) <*> (char ':' >> many space *> valueParser)

type MetaValue = [KeyValuePair]

metaValueParser :: Parser MetaValue
metaValueParser =
  (string "db" <|> string "model")
    >> many1 space
    >> many1 letter
    >> many1 space
    >> char '{'
    >> many space
    >> keyValueParser `sepBy` (char ',' <* many space)
      <* char '}'

type MetaBlock = [MetaValue]

metaBlockParser :: Parser MetaBlock
metaBlockParser =
  string "meta"
    >> many space
    >> char '{'
    >> many space
    >> (many space >> metaValueParser) `sepBy` (char ',' <* many space)
      <* char '}'

main :: IO ()
main = do
  codeStr <- readFile "data/code.txt"
  -- let parsedCode = parse codeParser "" codeStr
  -- print parsedCode
  -- let parsedCode = parse metaBlockParser "" codeStr
  let parsedCode = parse metaValueParser "" codeStr
  print parsedCode

-- TODO: Remove `try` everywhere where its not needed, because of possible performance impact.
