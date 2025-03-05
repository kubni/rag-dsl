module MyParser
  ( Text.Parsec.parse,
    -- codeParser,
    dslParser,
    -- Code,
    KeyValuePair,
    Value (..),
    -- Statement (..),
    MetaValue,
    MetaHeader,
  )
where

import Data.List (List)
import Text.Parsec
    ( char,
      digit,
      letter,
      space,
      string,
      sepBy,
      (<|>),
      many,
      many1,
      parse,
      try )
import Text.Parsec.String (Parser)

data Value = IntVal Int | StringVal String | BoolVal Bool | ListVal (List Value) | DictVal (List KeyValuePair) deriving (Eq, Show)

intValParser :: Parser Value
intValParser = IntVal <$> (read <$> many1 digit)

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

type KeyValuePair = (String, Value)

keyValueParser :: Parser KeyValuePair
keyValueParser = (,) <$> (allowedStringParser <* many space) <*> ((char ':' >> many space *> valueParser) <* many space)

type MetaHeader = (String, String)

metaHeaderParser :: Parser MetaHeader
metaHeaderParser = (,) <$> (string "db" <|> string "model") <*> (many1 space >> many1 letter <* many space)

type MetaValue = (MetaHeader, [KeyValuePair])

metaValueParser :: Parser MetaValue
metaValueParser =
  (,)
    <$> metaHeaderParser
    <*> ( many space
            >> char '{'
            >> many space
            >> keyValueParser `sepBy` (many space >> char ',' >> many space)
              <* many space
              <* char '}'
        )

-- type MetaBlock = [MetaValue]

-- metaBlockParser :: Parser MetaBlock
-- metaBlockParser =
--   string "meta"
--     >> many space
--     >> char '{'
--     >> many space
--     >> (many space >> metaValueParser <* many space) `sepBy` (many space >> char ',' >> many space)
--       <* many space
--       <* char '}'

dslParser :: Parser [MetaValue]
dslParser =
  many space
  >> char '{'
  >> many space
  >> (many space >> metaValueParser <* many space) `sepBy` (many space >> char ',' >> many space)
  <* many space
  <* char '}'

{-- TODO:
          1) Remove `try` everywhere where its not needed, because of possible performance impact.
          2) Reuse DictVal implementation in MetaVal if its possible

--}
