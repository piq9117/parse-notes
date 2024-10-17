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

import Control.Applicative.Combinators (manyTill)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    anySingleBut,
    eof,
    lookAhead,
    parseMaybe,
  )
import Text.Megaparsec.Char qualified

type Parser = Parsec () Text

data Note = Note
  { title :: !Text,
    body :: ![Text]
  }
  deriving stock (Show, Eq)

parseNotes :: Text -> [Note]
parseNotes content = fromMaybe [] (fmap catMaybes $ parseMaybe (many noteOrSkip) content)

noteOrSkip :: Parser (Maybe Note)
noteOrSkip =
  (Just <$> note) <|> (skipNonNoteContent $> Nothing)

skipNonNoteContent :: Parser ()
skipNonNoteContent = do
  void (manyTill anySingle (lookAhead commentStart))

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
