module Main where

import Text.Parsec
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

type Code = [Statement]

type Var = String

data Statement = Print Var | Assign Var Value deriving (Eq, Show)

data Value = IntVal Int | StringVal String deriving (Eq, Show)

intValParser :: Parser Value
intValParser = IntVal <$> (read <$> many1 digit)

stringValParser :: Parser Value
stringValParser = StringVal <$> many1 letter

printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)

assignParser :: Parser Statement
assignParser = Assign <$> (many1 letter) <*> (many1 space >> char '=' >> many1 space >> (try intValParser <|> stringValParser))

main :: IO ()
main = do
  print "Hello world"
