{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.Parser
  ( noteTitle,
    noteBody,
    noteBodyLine,
    note,
    Note (..),
    parseNotes,
  )
where

import Text.Megaparsec (Parsec, anySingleBut, eof, parseMaybe)
import Text.Megaparsec.Char qualified

type Parser = Parsec () Text

data Note = Note
  { title :: !Text,
    body :: ![Text]
  }
  deriving stock (Show, Eq)

parseNotes :: Text -> [Note]
parseNotes content = fromMaybe [] (parseMaybe (many note) content)

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
  void (Text.Megaparsec.Char.char '#')
  Text.Megaparsec.Char.space
  void (Text.Megaparsec.Char.string "Note")
  Text.Megaparsec.Char.space
  void (Text.Megaparsec.Char.char '[')
  title <- fmap toText $ many (anySingleBut ']')
  void (Text.Megaparsec.Char.char ']')
  noteBodyLineEnd
  pure title

noteBody :: Parser [Text]
noteBody = do
  lines <- many noteBodyLine
  pure lines

noteBodyLine :: Parser Text
noteBodyLine = do
  commentStart
  content <- fmap toText $ many (anySingleBut '\n')
  noteBodyLineEnd
  pure content

noteBodyLineEnd :: Parser ()
noteBodyLineEnd =
  void Text.Megaparsec.Char.newline
    <|> eof

commentStart :: Parser ()
commentStart = do
  void (Text.Megaparsec.Char.string "--")
  Text.Megaparsec.Char.space
  pure ()
