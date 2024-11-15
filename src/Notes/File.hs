{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit ((.|))
import Conduit qualified
import Data.UUID.V4 qualified
import Notes.DB (ManageDB)
import Notes.NoteTitle.Queries
  ( deleteFileContent,
    getFileContent,
    insertFileContent,
    insertNotes,
  )
import Notes.Parser qualified
import Notes.Render qualified
import Notes.Tracing
  ( ActiveSpan,
    MonadTracer,
    childOf,
    spanOpts,
    traced_,
  )
import UnliftIO.Async (concurrently_)

parseFile ::
  (ManageDB m, MonadTracer r m) =>
  ActiveSpan ->
  FilePath ->
  m ()
parseFile span filepath =
  traced_ (spanOpts "parse-file" $ childOf span) $ \_span -> do
    -- do not compose this with other conduit.
    -- so the file read will unlock
    void $
      Conduit.runConduitRes $
        Conduit.sourceFile filepath
          .| Conduit.mapMC (lift <<< insertFileContent span)
          .| Conduit.sinkList

    fileContent <- getFileContent span

    Conduit.runConduitRes $
      Conduit.yieldMany fileContent
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
          ( \parsedFileContent -> do
              concurrently_
                (Notes.Render.renderFileContentsToFile span filepath parsedFileContent)
                ( lift $
                    insertNotes span (Notes.Parser.toNotes parsedFileContent)
                )

              lift (deleteFileContent span)
          )
    pure ()
