module Main where

import Text.Parsec (ParseError)
import Text.Parsec.Prim (runP)

import Lib (parseBody, Parser)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = runP p "" ""

main :: IO ()
main = do
  text <- readFile "test.md"
  print $ regularParse parseBody text
