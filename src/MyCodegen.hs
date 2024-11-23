module MyCodegen (keyValuePairToString, valueToPython, statementToPython) where

import Data.List (intercalate)
import MyParser (KeyValuePair, Statement (..), Value (..))

keyValuePairToString :: KeyValuePair -> String
keyValuePairToString (k, v) = k ++ ": " ++ valueToPython v

valueToPython :: Value -> String
valueToPython value = case value of
  IntVal v -> show v
  StringVal v -> v
  BoolVal v -> show v
  ListVal l -> "[" ++ intercalate ", " (map valueToPython l) ++ "]"
  DictVal d -> "{" ++ intercalate ", " (map keyValuePairToString d) ++ "}"

statementToPython :: Statement -> String
statementToPython statement = case statement of
  Assign var value -> var ++ " = " ++ valueToPython value
  Print var -> "print(" ++ var ++ ")"
