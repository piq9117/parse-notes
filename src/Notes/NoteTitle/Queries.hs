{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Notes.NoteTitle.Queries
  ( NoteTitleInput (..),
    insertNoteTitles,
  )
where

import Database.Beam
  ( default_,
    insert,
    insertExpressions,
    runInsert,
    val_,
  )
import Notes.DB (ManageDB (..))
import Notes.Model (Database (..), database)
import Notes.Models.NoteTitle (NoteTitleT (..))
import Prelude hiding (id)

data NoteTitleInput = NoteTitleInput
  { title :: Text,
    hash :: Text
  }

insertNoteTitles ::
  (MonadIO m, ManageDB m) =>
  [NoteTitleInput] ->
  m ()
insertNoteTitles noteTitles =
  runQuery $
    runInsert $
      insert
        database.noteTitles
        ( insertExpressions $
            [ NoteTitle
                { id = default_,
                  title = val_ noteTitle.title,
                  hash = val_ noteTitle.hash,
                  createdAt = default_,
                  updatedAt = default_
                }
              | noteTitle <- noteTitles
            ]
        )
