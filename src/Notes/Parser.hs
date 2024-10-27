{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.Parser
  ( Note (..),
    NoteBody (..),
    NoteTitle (..),
    FileContent (..),
    noteTitle,
    noteBody,
    noteBodyLine,
    noteParser,
    parseNotes,
    noteBodyLineParser,
    noteBodyParser,
    noteTitleParser,
    noteBodyIdParser,
    uuid,
    fileParser,
    fileNoteParser,
    blanklineParser,
  )
where

import Control.Applicative.Combinators (count, manyTill)
import Data.UUID qualified
import Text.Megaparsec
  ( Parsec,
    anySingle,
    anySingleBut,
    lookAhead,
    parseMaybe,
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified
import Text.PrettyPrint qualified
import Text.PrettyPrint.HughesPJClass (Pretty (..))

type Parser = Parsec () Text

data FileContent
  = NoteContent Note
  | Blankline
  deriving stock (Eq, Show)

fileParser :: Parser [FileContent]
fileParser = many fileNoteParser

fileNoteParser :: Parser FileContent
fileNoteParser =
  fmap NoteContent noteParser <|> blanklineParser

blanklineParser :: Parser FileContent
blanklineParser = fmap (const Blankline) Text.Megaparsec.Char.eol

data Note = Note
  { title :: !NoteTitle,
    body :: ![NoteBody]
  }
  deriving stock (Show, Eq)

data NonNote = NonNote !Text
  deriving stock (Show, Eq)

instance Pretty Note where
  pPrint note = pPrint note

parseNotes :: Text -> [Note]
parseNotes content = fromMaybe [] (fmap catMaybes $ parseMaybe (many noteOrSkip) content)

noteOrSkip :: Parser (Maybe Note)
noteOrSkip =
  (Just <$> noteParser) <|> (skipNonNoteContent $> Nothing)

skipNonNoteContent :: Parser ()
skipNonNoteContent = do
  void (manyTill anySingle (lookAhead commentStart))

noteParser :: Parser Note
noteParser = do
  title <- noteTitleParser
  body <- noteBodyParser
  noteBodyLineEnd
  pure
    Note
      { title,
        body
      }

data NoteTitle = NoteTitle !Text
  deriving stock (Eq, Show)

instance Pretty NoteTitle where
  -- render:
  -- # Note [This is a title]
  pPrint (NoteTitle title) =
    Text.PrettyPrint.text "-- # Note ["
      Text.PrettyPrint.<+> Text.PrettyPrint.text (toString title)
      Text.PrettyPrint.<+> Text.PrettyPrint.text "]"

instance ToText NoteTitle where
  toText (NoteTitle title) = title

data NoteBody
  = BodyContent !Text
  | BodyId !Text
  deriving stock (Eq, Show)

instance ToText NoteBody where
  toText (BodyContent content) = content
  toText (BodyId bodyId) = bodyId

instance Pretty NoteBody where
  -- render:
  -- This is the body
  pPrint (BodyContent content) =
    Text.PrettyPrint.text "--"
      Text.PrettyPrint.<+> Text.PrettyPrint.space
      Text.PrettyPrint.<+> Text.PrettyPrint.text (toString content)
  -- render:
  -- id:b67941d1-222b-4cfa-ba37-d02973aa2141
  pPrint (BodyId bodyId) =
    Text.PrettyPrint.text "--"
      Text.PrettyPrint.<+> Text.PrettyPrint.space
      Text.PrettyPrint.<+> Text.PrettyPrint.text "id:"
      Text.PrettyPrint.<+> Text.PrettyPrint.text (toString bodyId)

noteTitleParser :: Parser NoteTitle
noteTitleParser = fmap NoteTitle noteTitle

noteBodyParser :: Parser [NoteBody]
noteBodyParser = (many noteBodyLineParser)

noteBodyLineParser :: Parser NoteBody
noteBodyLineParser =
  (Text.Megaparsec.try noteBodyIdParser <|> fmap BodyContent noteBodyLine)

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
noteBody = many noteBodyLine

noteBodyLine :: Parser Text
noteBodyLine = do
  commentStart
  line <- fmap toText $ many (anySingleBut '\n')
  noteBodyLineEnd
  pure line

noteBodyLineEnd :: Parser ()
noteBodyLineEnd =
  void Text.Megaparsec.Char.newline
    <|> Text.Megaparsec.eof

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
