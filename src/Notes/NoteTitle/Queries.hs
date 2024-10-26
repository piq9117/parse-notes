{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.NoteTitle.Queries
  ( NoteTitleInput (..),
    insertNoteTitles,
  )
where

import Database.Beam
  ( default_,
    insertExpressions,
    runInsert,
    val_,
    (<-.)
  )
import Database.Beam.Backend.SQL.BeamExtensions
  ( conflictingFields,
    insertOnConflict,
    onConflictUpdateSet,
  )
import Notes.DB (ManageDB (..))
import Notes.Hash (hashContent)
import Notes.Model (Database (..), database)
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
    hash :: Text
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
        insertOnConflict
          database.noteTitles
          ( insertExpressions $
              [ NoteTitle
                  { id = default_,
                    title = val_ noteTitle.title,
                    hash = val_ (hashContent noteTitle.hash),
                    createdAt = default_,
                    updatedAt = default_
                  }
                | noteTitle <- noteTitles
              ]
          )
          (conflictingFields $ \noteTitle -> noteTitle.hash)
          (onConflictUpdateSet $ \current new -> 
            mconcat
              [ current.title <-. new.title,
                current.hash <-. new.hash,
                current.updatedAt <-. new.updatedAt
              ]
          )
