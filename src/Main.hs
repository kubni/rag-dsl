module Main where

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse, runParser)

main :: IO ()
main = do
  print "Hello cruel world"
