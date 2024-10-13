{-# LANGUAGE OverloadedStrings #-}

module Notes.Parser (title, body) where

import Data.Char (isPunctuation)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, parse, satisfy)
import Text.Megaparsec.Char qualified
import Text.Megaparsec.Char.Lexer qualified
import Text.Megaparsec.Error (ParseErrorBundle (..))

type Parser = Parsec () Text

data Note = Note
  { title :: !Text,
    body :: ![Text]
  }

parseNote :: Parser Note
parseNote = do
  title <- noteTitle
  body <- many (noteBody <* Text.Megaparsec.Char.eol)
  pure
    Note
      { title,
        body
      }

noteTitle :: Parser Text
noteTitle = do
  commentStart
  Text.Megaparsec.Char.space
  Text.Megaparsec.Char.char '#'
  Text.Megaparsec.Char.space
  Text.Megaparsec.Char.string "Note"
  Text.Megaparsec.Char.space
  Text.Megaparsec.Char.char '['
  fmap toText $ many (anySingleBut ']')

noteBody :: Parser Text
noteBody = do
  commentStart
  fmap toText $ many anySingle

commentStart :: Parser ()
commentStart = do
  Text.Megaparsec.Char.char '-'
  Text.Megaparsec.Char.char '-'
  Text.Megaparsec.Char.space
  pure ()
