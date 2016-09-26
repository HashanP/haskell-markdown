module Lib
( parseBody
, parseText
, parseEmph
, parseItalic
, parseParagraph
, Block(..)
, Parser
, regularParse
) where

import Text.Parsec (parse, try, Parsec)
import Text.Parsec.Combinator (many1, count, choice, optional, optionMaybe, notFollowedBy, lookAhead)
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Prim (getState, setState)
import Text.Parsec.Char (letter, char)
import Control.Applicative ((<*), (<|>), many)
import Control.Monad (void)
import Debug.Trace (trace)
import Text.Parsec (ParseError)
import Text.Parsec.Prim (runP)

data Block
  = Paragraph [Block]
  | Blockquote [Block]
  | Text String
  | Emph [Block]
  | Italic [Block]
  | Header Int [Block]
  deriving (Show, Eq)

type Parser = Parsec String String

getParsers :: String -> [Parser Block]
getParsers state = d ++ c ++ [parseText] 
  where c | 'I' `notElem` state = [try parseItalic]
          | otherwise = []
        d | 'E' `notElem` state = [try parseEmph]
          | otherwise = []

parseText :: Parser Block
parseText = do
  text <- many1 parseLine 
  return $ Text (concat $ text)

parseLine :: Parser String
parseLine = do
  void $ many (char ' ') -- Spaces at the beginning of a line are ignored
  first <- letter <|> char ' ' -- First char must be a letter or a character
  rest <- many (letter <|> char ' ') 
  newline <- optionMaybe $ char '\n'
  return $ case newline of 
    Just _ -> first:rest ++ " "
    Nothing -> first:rest

parseParagraph :: Parser Block
parseParagraph = do
  blocks <- many1 $ choice [try parseEmph, parseItalic, parseText]
  void $ many1 (char '\n')
  return $ Paragraph blocks

{-
The markdown specification mandates, a line beginning with 0-3 spaces
and then ">" constitutes a blockquote.
-}
parseBlockquote :: Parser Block
parseBlockquote = do
  void $ count 3 (optional (char ' '))
  void $ char '>'
  void $ many (char '_')
  blocks <- many1 $ choice [try parseEmph, parseItalic, parseText]
  return $ Blockquote blocks

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
 paras <- many1 $ choice [try parseBlockquote, parseParagraph]
 return paras

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = runP p "" ""
