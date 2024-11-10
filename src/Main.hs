module Main where

import Text.Parsec (letter, many1, space, string)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

type Code = [Statement]

type Var = String

data Statement = Print Var | Assign Var Value deriving (Eq, Show)

data Value = IntVal Int | StringVal String deriving (Eq, Show)

printParser :: Parser Statement
printParser = Print <$> (string "print" >> many1 space >> many1 letter)

main :: IO ()
main = do
  print "Hello world"
