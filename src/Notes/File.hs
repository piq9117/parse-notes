{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit ((.|))
import Conduit qualified
import Notes.DB (ManageDB)
-- import Notes.NoteTitle.Queries (NoteTitleInput (..), insertNoteTitles)
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
