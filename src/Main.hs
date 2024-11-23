module Main where

import MyParser (metaBlockParser, parse)

main :: IO ()
main = do
  codeStr <- readFile "data/code.txt"
  let parsedCode = parse metaBlockParser "" codeStr
  print parsedCode
