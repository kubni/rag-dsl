module Main where

import MyCodegen (codegen)
import MyParser (dslParser, parse)

main :: IO ()
main = do
  codeStr <- readFile "data/code.txt"
  let parsedCode = parse dslParser "" codeStr -- type: [MetaValue]
  case parsedCode of
    Left msg -> print msg -- TODO: Handle this better
    Right metaBlock -> print $ codegen metaBlock
