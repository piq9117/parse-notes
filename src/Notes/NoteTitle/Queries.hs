{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.NoteTitle.Queries
  ( NoteTitleInput (..),
    insertNoteTitles,
  )
where

import Data.UUID (UUID)
import Database.Beam
  ( default_,
    insert,
    insertExpressions,
    runInsert,
    val_,
  )
import Notes.DB (ManageDB (..))
import Notes.Model (Database (..), database)
import Notes.Models.Instances ()
import Notes.Models.NoteTitle (NoteTitleT (..))
import Notes.Tracing
  ( ActiveSpan,
    MonadTracer,
    TagVal (..),
    addTag,
    childOf,
    spanOpts,
    traced_,
  )
import Prelude hiding (id)

data NoteTitleInput = NoteTitleInput
  { title :: Text,
    noteId :: UUID
  }
  deriving (Show)

insertNoteTitles ::
  (ManageDB m, MonadTracer r m) =>
  ActiveSpan ->
  [NoteTitleInput] ->
  m ()
insertNoteTitles span noteTitles =
  traced_ (spanOpts "insert-note-titles" $ childOf span) $ \span -> do
    addTag span ("titles-length", IntT $ fromIntegral $ length $ noteTitles)
    runQuery span $
      runInsert $
        insert
          database.noteTitles
          ( insertExpressions $
              [ NoteTitle
                  { id = default_,
                    title = val_ noteTitle.title,
                    noteId = val_ noteTitle.noteId,
                    createdAt = default_,
                    updatedAt = default_
                  }
                | noteTitle <- noteTitles
              ]
          )
