{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit ((.|))
import Conduit qualified
import Data.UUID.V4 qualified
import Notes.DB (ManageDB)
-- import Notes.NoteTitle.Queries (NoteTitleInput (..), insertNoteTitles)
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
        .| Conduit.mapM_C
          ( \notes -> do
              print notes
              parsedNotes <-
                Notes.Parser.parseFile
                  span
                  (decodeUtf8 notes)
                  Notes.Parser.GenerateBodyId
                    { Notes.Parser.generateBodyId = \_span -> liftIO Data.UUID.V4.nextRandom
                    }
              print parsedNotes
              -- lift $
              --   insertNoteTitles
              --     span
              --     [ NoteTitleInput
              --         { title = toText note.title,
              --           hash = toText note.title
              --         }
              --       | note <- notes
              --     ]
          )
