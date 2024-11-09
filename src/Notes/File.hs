{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit ((.|))
import Conduit qualified
import Data.UUID qualified
import Data.UUID.V4 qualified
import Notes.DB (ManageDB)
import Notes.NoteTitle.Queries (NoteTitleInput (..), insertNoteTitles)
import Notes.Parser qualified
import Notes.Tracing
  ( ActiveSpan,
    MonadTracer,
    childOf,
    spanOpts,
    traced_,
  )

parseFile ::
  (ManageDB m, MonadTracer r m) =>
  ActiveSpan ->
  FilePath ->
  m ()
parseFile span filepath =
  traced_ (spanOpts "parse-file" $ childOf span) $ \_span ->
    Conduit.runConduitRes $
      Conduit.sourceFile filepath
        .| Conduit.mapMC
          ( \fileContent ->
              lift $
                Notes.Parser.parseFileM
                  span
                  Notes.Parser.GenerateBodyId
                    { generate = \_ -> liftIO Data.UUID.V4.nextRandom
                    }
                  (decodeUtf8 fileContent)
          )
        .| Conduit.mapM_C
          ( \parsedFileContent ->
              lift $
                insertNoteTitles
                  span
                  [ NoteTitleInput
                      { title = case fileContent of
                          Notes.Parser.NoteContent note ->
                            (\(Notes.Parser.NoteTitle title) -> title) note.title
                          _ -> "",
                        noteId =
                          case fileContent of
                            Notes.Parser.NoteContent note ->
                              fromMaybe Data.UUID.nil (Data.UUID.fromText <=< (fmap (\(Notes.Parser.NoteId noteId) -> noteId)) $ note.id)
                            _ -> Data.UUID.nil
                      }
                    | fileContent <- parsedFileContent
                  ]
          )
