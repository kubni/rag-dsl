module Main where

import GHC.Base (List)
import Text.Parsec
import Text.Parsec.String (Parser)

type Code = [Statement]

type Var = String

data Statement = Print Var | Assign Var Value deriving (Eq, Show)

data Value = IntVal Int | StringVal String | BoolVal Bool | ListVal (List Value) | DictVal (List KeyValuePair) deriving (Eq, Show)

printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)

intValParser :: Parser Value
intValParser = IntVal <$> (read <$> many1 digit)

-- TODO: This allows strings like "123foo", which aren't supported in Python
-- TODO: I can define my own string as a letter or underscore followed by letters, numbers, underscores, but how do we combine it with this noneOf below?

allowedStringParser :: Parser String
allowedStringParser = (:) <$> (letter <|> char '_') <*> many (letter <|> char '_' <|> digit)

stringValParser :: Parser Value
stringValParser = StringVal <$> ((char '"' <|> char '\'') >> allowedStringParser <* (char '"' <|> char '\''))

boolValParser :: Parser Value
boolValParser = BoolVal <$> ((True <$ string "True") <|> (False <$ string "False"))

listValParser :: Parser Value
listValParser = ListVal <$> (char '[' >> ((valueParser <* many space) `sepBy` (char ',' <* many space)) <* char ']')

dictValParser :: Parser Value
dictValParser = DictVal <$> (char '{' >> many space >> keyValueParser `sepBy` (char ',' >> many space) <* char '}')

valueParser :: Parser Value
valueParser = try intValParser <|> boolValParser <|> stringValParser <|> listValParser <|> dictValParser

assignParser :: Parser Statement
assignParser = Assign <$> (many1 letter) <*> (many1 space >> char '=' >> many1 space >> valueParser)

statementParser :: Parser Statement
statementParser = try printParser <|> try assignParser

codeParser :: Parser Code
codeParser = spaces >> many (statementParser <* many1 space) <* eof

type KeyValuePair = (String, Value)

keyValueParser :: Parser KeyValuePair
keyValueParser = (,) <$> (allowedStringParser <* many space) <*> ((char ':' >> many space *> valueParser) <* many space)

type MetaValue = [KeyValuePair]

metaValueParser :: Parser MetaValue
metaValueParser =
  (string "db" <|> string "model")
    >> many1 space
    >> many1 letter
    >> many1 space
    >> char '{'
    >> many space
    >> keyValueParser `sepBy` (many space >> char ',' >> many space)
      <* many space
      <* char '}'

type MetaBlock = [MetaValue]

metaBlockParser :: Parser MetaBlock
metaBlockParser =
  string "meta"
    >> many space
    >> char '{'
    >> many space
    >> (many space >> metaValueParser <* many space) `sepBy` (many space >> char ',' >> many space)
      <* many space
      <* char '}'

main :: IO ()
main = do
  codeStr <- readFile "data/code.txt"
  -- let parsedCode = parse codeParser "" codeStr
  -- print parsedCode
  -- let parsedCode = parse metaBlockParser "" codeStr
  let parsedCode = parse metaBlockParser "" codeStr
  print parsedCode

{-- TODO:
          1) Remove `try` everywhere where its not needed, because of possible performance impact.
          2) Reuse DictVal implementation in MetaVal if its possible

--}
