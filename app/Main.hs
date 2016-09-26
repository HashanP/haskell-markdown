module Main where

import Lib (parseBody, Parser, regularParse)

main :: IO ()
main = do
  text <- readFile "test.md"
  print $ regularParse parseBody text
