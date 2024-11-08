{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.Parser
  ( Note (..),
    NoteBody (..),
    NoteTitle (..),
    FileContent (..),
    NonNote (..),
    BodyId (..),
    GenerateBodyId (..),
    parseFile,
    parseFileM,
    noteTitle,
    noteBody,
    noteBodyLine,
    noteParser,
    noteBodyLineParser,
    noteBodyParser,
    noteTitleParser,
    noteBodyIdParser,
    uuid,
    uuidBodyLine,
    fileParser,
    fileNoteParser,
    blanklineParser,
    nonNoteParser,
    nonNoteLine,
  )
where

import Control.Applicative.Combinators (between, count, manyTill)
import Data.UUID qualified
import Notes.Tracing (ActiveSpan, MonadTracer, childOf, spanOpts, traced_)
import Text.Megaparsec
  ( Parsec,
    anySingleBut,
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified
import Text.PrettyPrint qualified
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Prelude hiding (id)

type Parser = Parsec () Text

data FileContent
  = NoteContent Note
  | NonNoteContent !NonNote
  | Blankline
  deriving stock (Eq, Show)

instance Pretty FileContent where
  pPrint (NoteContent note) = pPrint note
  pPrint Blankline =
    Text.PrettyPrint.text "\n"
  pPrint (NonNoteContent nonNotes) =
    pPrint nonNotes

newtype GenerateBodyId m = GenerateBodyId
  { generate :: ActiveSpan -> m Data.UUID.UUID
  }

parseFile :: Text -> [FileContent]
parseFile content = fromMaybe [] (Text.Megaparsec.parseMaybe fileParser content)

parseFileM :: (MonadTracer r m) => ActiveSpan -> GenerateBodyId m -> Text -> m [FileContent]
parseFileM span generateBodyId content =
  traced_ (spanOpts "parse-file-m" $ childOf span) $ \span -> do
    fileParser <- fileParserM span generateBodyId
    pure $ fromMaybe [] (Text.Megaparsec.parseMaybe fileParser content)

fileParserM :: (MonadTracer r m) => ActiveSpan -> GenerateBodyId m -> m (Parser [FileContent])
fileParserM span generateBodyId =
  traced_ (spanOpts "file-parser-m" $ childOf span) $ \span -> do
    fileNoteParser <- fileNoteParserM span generateBodyId
    pure $
      many fileNoteParser

fileParser :: Parser [FileContent]
fileParser = many fileNoteParser

fileNoteParserM :: (MonadTracer r m) => ActiveSpan -> GenerateBodyId m -> m (Parser FileContent)
fileNoteParserM span generateBodyId =
  traced_ (spanOpts "file-note-parser-m" $ childOf span) $ \span -> do
    noteParser <- noteParserM span generateBodyId
    pure $ do
      fmap NoteContent noteParser
        <|> (fmap NonNoteContent nonNoteParser)
        <|> blanklineParser

fileNoteParser :: Parser FileContent
fileNoteParser =
  fmap NoteContent noteParser
    <|> (fmap NonNoteContent nonNoteParser)
    <|> blanklineParser

blanklineParser :: Parser FileContent
blanklineParser = fmap (const Blankline) Text.Megaparsec.Char.eol

data Note = Note
  { title :: !NoteTitle,
    body :: ![NoteBody],
    id :: Maybe BodyId
  }
  deriving stock (Show, Eq)

instance Pretty Note where
  pPrint note =
    pPrint note.title
      Text.PrettyPrint.<> (Text.PrettyPrint.hcat $ fmap pPrint note.body)
      Text.PrettyPrint.<> (maybe mempty pPrint note.id)

noteParserM :: (MonadTracer r m) => ActiveSpan -> GenerateBodyId m -> m (Parser Note)
noteParserM span generateBodyId =
  traced_ (spanOpts "note-parser-m" $ childOf span) $ \span -> do
    generatedBodyId <- generateBodyId.generate span
    pure $ do
      title <- noteTitleParser
      body <- noteBodyParser
      bodyId <- optional noteBodyIdParser
      pure $
        Note
          { title,
            body,
            id = Just $ fromMaybe (BodyId $ Data.UUID.toText generatedBodyId) bodyId
          }

noteParser :: Parser Note
noteParser = do
  title <- noteTitleParser
  body <- noteBodyParser
  id <- optional noteBodyIdParser
  pure
    Note
      { title,
        body,
        id
      }

data NoteTitle = NoteTitle !Text
  deriving stock (Eq, Show)

instance Pretty NoteTitle where
  -- render:
  -- # Note [This is a title]
  pPrint (NoteTitle title) =
    Text.PrettyPrint.text "-- # Note ["
      Text.PrettyPrint.<> Text.PrettyPrint.text (toString title)
      Text.PrettyPrint.<> Text.PrettyPrint.text "]"
      Text.PrettyPrint.<> Text.PrettyPrint.text "\n"

instance ToText NoteTitle where
  toText (NoteTitle title) = title

data NoteBody
  = BodyContent !Text
  deriving stock (Eq, Show)

data BodyId = BodyId !Text
  deriving stock (Eq, Show)

instance Pretty BodyId where
  -- render:
  -- id:b67941d1-222b-4cfa-ba37-d02973aa2141
  pPrint (BodyId bodyId) =
    Text.PrettyPrint.text "--"
      Text.PrettyPrint.<> Text.PrettyPrint.space
      Text.PrettyPrint.<> Text.PrettyPrint.text "id:"
      Text.PrettyPrint.<> Text.PrettyPrint.text (toString bodyId)
      Text.PrettyPrint.<> Text.PrettyPrint.text "\n"

instance ToText NoteBody where
  toText (BodyContent content) = content

instance Pretty NoteBody where
  -- render:
  -- This is the body
  pPrint (BodyContent content) =
    Text.PrettyPrint.text "--"
      Text.PrettyPrint.<> Text.PrettyPrint.space
      Text.PrettyPrint.<> Text.PrettyPrint.text (toString content)
      Text.PrettyPrint.<> Text.PrettyPrint.text "\n"

noteTitleParser :: Parser NoteTitle
noteTitleParser = fmap NoteTitle noteTitle

noteBodyParser :: Parser [NoteBody]
noteBodyParser = (many noteBodyLineParser)

noteBodyLineParser :: Parser NoteBody
noteBodyLineParser = fmap BodyContent noteBodyLine

noteBodyIdParser :: Parser BodyId
noteBodyIdParser = do
  line <- uuidBodyLine
  pure $ BodyId line

noteTitle :: Parser Text
noteTitle = do
  commentStart
  Text.Megaparsec.Char.space
  void (Text.Megaparsec.Char.char '#')
  Text.Megaparsec.Char.space
  void (Text.Megaparsec.Char.string "Note")
  Text.Megaparsec.Char.space
  title <-
    between
      (Text.Megaparsec.Char.char '[')
      (Text.Megaparsec.Char.char ']')
      (fmap toText $ many $ anySingleBut ']')
  noteBodyLineEnd
  pure title

noteBody :: Parser [Text]
noteBody = many noteBodyLine

noteBodyLine :: Parser Text
noteBodyLine = do
  Text.Megaparsec.notFollowedBy (Text.Megaparsec.Char.string "-- id:")
  commentStart
  line <- fmap toText $ manyTill (anySingleBut '\n') noteBodyLineEnd
  pure line

noteBodyLineEnd :: Parser ()
noteBodyLineEnd =
  ( void Text.Megaparsec.Char.newline
      <|> Text.Megaparsec.eof
  )

commentStart :: Parser ()
commentStart = do
  void (Text.Megaparsec.Char.string "--")
  Text.Megaparsec.Char.space
  pure ()

uuidBodyLine :: Parser Text
uuidBodyLine = do
  commentStart
  void (Text.Megaparsec.try $ Text.Megaparsec.Char.string "id:")
  uuid

uuid :: Parser Text
uuid = do
  uuidContent <-
    (count 36 (Text.Megaparsec.Char.alphaNumChar <|> Text.Megaparsec.Char.char '-'))
  case Data.UUID.fromText (toText uuidContent) of
    Nothing -> fail "Invalid UUID format"
    Just uuidContent -> pure $ Data.UUID.toText uuidContent

data NonNote = NonNote !Text
  deriving stock (Show, Eq)

instance Pretty NonNote where
  pPrint (NonNote nonNote) =
    Text.PrettyPrint.text (toString nonNote)
      Text.PrettyPrint.<> Text.PrettyPrint.text "\n"

nonNoteParser :: Parser NonNote
nonNoteParser = fmap NonNote nonNoteLine

nonNoteLine :: Parser Text
nonNoteLine = do
  Text.Megaparsec.notFollowedBy commentStart
  line <- Text.Megaparsec.takeWhile1P Nothing (/= '\n')
  noteBodyLineEnd
  pure line
