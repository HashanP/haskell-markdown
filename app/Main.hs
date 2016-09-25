module Main where

import Text.Parsec (parse, try)
import Text.Parsec.Combinator (many1, count, choice, optional, notFollowedBy, lookAhead)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (letter, char)
import Control.Applicative ((<*), (<|>), many)
import Control.Monad (void)
import Lib

data Block
  = Paragraph [Block]
  | Text String
  | Emph [Block]
  | Italic [Block]
  | Header Int [Block]
  deriving (Show)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseText :: Parser Block
parseText = do
  text <- many1 (letter <|> char ' ' <|> try (char '\n' <* notFollowedBy (char '\n') <* lookAhead letter)) 
  return $ Text (map change text)
  where change '\n' = ' '
        change x = x

parseParagraph :: Parser Block
parseParagraph = do
  blocks <- many1 $ choice [parseText, parseEmph, parseItalic]
  void $ many (char '\n')
  return $ Paragraph blocks

parseEmph :: Parser Block
parseEmph = do
  void $ count 2 (char '*')
  blocks <- many1 $ choice [parseText, try parseItalic]
  void $ count 2 (char '*')
  return $ Emph blocks

parseItalic :: Parser Block
parseItalic = do
  void $ char '*'
  blocks <- many1 $ choice [parseText]
  void $ char '*'
  return $ Italic blocks

parseBody :: Parser [Block]
parseBody = do
  blocks <- many1 $ parseParagraph
  return blocks

main :: IO ()
main = do
  text <- readFile "test.md"
  print $ regularParse parseBody text
