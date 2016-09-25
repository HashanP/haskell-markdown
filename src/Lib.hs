module Lib
( parseBody
, Parser
) where

import Text.Parsec (parse, try, Parsec)
import Text.Parsec.Combinator (many1, count, choice, optional, notFollowedBy, lookAhead)
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Prim (getState, setState)
import Text.Parsec.Char (letter, char)
import Control.Applicative ((<*), (<|>), many)
import Control.Monad (void)
import Debug.Trace (trace)

data Block
  = Paragraph [Block]
  | Text String
  | Emph [Block]
  | Italic [Block]
  | Header Int [Block]
  deriving (Show)

type Parser = Parsec String String

getParsers :: String -> [Parser Block]
getParsers state = d ++ c ++ [parseText] 
  where c | 'I' `notElem` state = [try parseItalic]
          | otherwise = []
        d | 'E' `notElem` state = [try parseEmph]
          | otherwise = []

parseText :: Parser Block
parseText = do
  text <- many1 (letter <|> char ' ' <|> try (char '\n' <* notFollowedBy (char '\n') <* lookAhead letter)) 
  return $ Text (map change text)
  where change '\n' = ' '
        change x = x

parseParagraph :: Parser Block
parseParagraph = do
  blocks <- many1 $ choice [try parseEmph, parseItalic, parseText]
  void $ many (char '\n')
  return $ Paragraph blocks

parseEmph :: Parser Block
parseEmph = do
  void $ count 2 (char '*')
  s <- getState
  setState $ s ++ "E"
  blocks <- many1 $ choice (getParsers (s ++ "E"))
  init <$> getState >>= setState
  void $ count 2 (char '*')
  return $ Emph blocks

parseItalic :: Parser Block
parseItalic = do
  void $ char '*'
  s <- getState
  setState $ s ++ "I"
  blocks <- many1 $ choice (getParsers (s ++ "I"))
  (init <$> getState) >>= setState
  void $ char '*'
  return $ Italic blocks

parseBody :: Parser [Block]
parseBody = do
 paras <- many1 $ parseParagraph 
 return paras
