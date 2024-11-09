{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Notes.Models.NoteTitle
  ( NoteTitleT (..),
    NoteTitle,
    PrimaryKey (NoteTitleId),
    noteTitleSettings,
  )
where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
  ( Beamable,
    Columnar,
    DatabaseEntity,
    EntityModification,
    Table (..),
    TableEntity,
    modifyTableFields,
    setEntityName,
    tableModification,
  )
import Prelude hiding (id)

data NoteTitleT f = NoteTitle
  { id :: Columnar f Int64,
    title :: Columnar f Text,
    noteId :: Columnar f UUID,
    createdAt :: Columnar f UTCTime,
    updatedAt :: Columnar f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type NoteTitle = NoteTitleT Identity

instance Table NoteTitleT where
  newtype PrimaryKey NoteTitleT f = NoteTitleId
    { unNoteTitleId :: Columnar f Int64
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = NoteTitleId <<< id

noteTitleSettings :: EntityModification (DatabaseEntity be db) be (TableEntity NoteTitleT)
noteTitleSettings =
  setEntityName "note_titles"
    <> modifyTableFields
      tableModification
        { id = "id",
          title = "title",
          noteId = "note_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
