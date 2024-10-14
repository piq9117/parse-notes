{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.Parser
  ( noteTitle,
    noteBody,
    noteBodyLine,
    note,
    Note (..),
  )
where

import Data.Char (isPunctuation)
import Text.Megaparsec (Parsec, anySingle, anySingleBut, eof, parse, satisfy)
import Text.Megaparsec.Char qualified
import Text.Megaparsec.Char.Lexer qualified
import Text.Megaparsec.Error (ParseErrorBundle (..))

type Parser = Parsec () Text

data Note = Note
  { title :: !Text,
    body :: ![Text]
  }
  deriving stock (Show, Eq)

note :: Parser Note
note = do
  title <- noteTitle
  body <- noteBody
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
  title <- fmap toText $ many (anySingleBut ']')
  Text.Megaparsec.Char.char ']'
  pure title

noteBody :: Parser [Text]
noteBody = many noteBodyLine

noteBodyLine :: Parser Text
noteBodyLine = do
  noteBodyLineEnd
  commentStart
  fmap toText $ many (anySingleBut '\n')

noteBodyLineEnd :: Parser ()
noteBodyLineEnd =
  void Text.Megaparsec.Char.newline
    <|> void Text.Megaparsec.Char.eol
    <|> eof

commentStart :: Parser ()
commentStart = do
  Text.Megaparsec.Char.char '-'
  Text.Megaparsec.Char.char '-'
  Text.Megaparsec.Char.space
  pure ()
