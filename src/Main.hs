module Main where

import MyCodegen (processMetaValue)
import MyParser (metaBlockParser, parse)

main :: IO ()
main = do
  codeStr <- readFile "data/code.txt"
  let parsedCode = parse metaBlockParser "" codeStr -- type: MetaBlock <=> [MetaValue]
  case parsedCode of
    Left msg -> print msg -- TODO: Handle this better
    Right metaBlock -> print $ processMetaValue <$> metaBlock
