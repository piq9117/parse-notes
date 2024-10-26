{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.Parser
  ( Note (..),
    NoteBody (..),
    NoteTitle (..),
    noteTitle,
    noteBody,
    noteBodyLine,
    note,
    parseNotes,
    noteBodyLineParser,
    noteTitleParser,
    noteBodyIdParser,
    uuid,
  )
where

import Control.Applicative.Combinators (count, manyTill)
import Data.UUID qualified
import Text.Megaparsec
  ( Parsec,
    anySingle,
    anySingleBut,
    eof,
    lookAhead,
    parseMaybe,
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

type Parser = Parsec () Text

data Note = Note
  { title :: !NoteTitle,
    body :: ![NoteBody]
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
  title <- noteTitleParser
  body <- noteBodyParser
  pure
    Note
      { title,
        body
      }

data NoteTitle = NoteTitle !Text
  deriving stock (Eq, Show)

instance ToText NoteTitle where
  toText (NoteTitle title) = title

data NoteBody
  = BodyContent !Text
  | BodyId !Text
  deriving stock (Eq, Show)

instance ToText NoteBody where
  toText (BodyContent content) = content
  toText (BodyId bodyId) = bodyId

noteTitleParser :: Parser NoteTitle
noteTitleParser = fmap NoteTitle noteTitle

noteBodyParser :: Parser [NoteBody]
noteBodyParser = many noteBodyLineParser

noteBodyLineParser :: Parser NoteBody
noteBodyLineParser =
  Text.Megaparsec.try noteBodyIdParser <|> fmap BodyContent noteBodyLine

noteBodyIdParser :: Parser NoteBody
noteBodyIdParser = do
  commentStart
  line <- uuidBodyLine
  pure (BodyId line)

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

uuidBodyLine :: Parser Text
uuidBodyLine = do
  void (Text.Megaparsec.Char.string "id:")
  uuid

uuid :: Parser Text
uuid = do
  uuidContent <-
    (count 36 (Text.Megaparsec.Char.alphaNumChar <|> Text.Megaparsec.Char.char '-'))
  case Data.UUID.fromText (toText uuidContent) of
    Nothing -> fail "Invalid UUID format"
    Just uuidContent -> pure $ Data.UUID.toText uuidContent
