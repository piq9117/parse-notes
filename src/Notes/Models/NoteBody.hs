{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Notes.Models.NoteBody
  ( NoteBodyT (..),
    NoteBody,
    PrimaryKey (NoteBodyId),
    noteBodySettings,
  )
where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
  ( Beamable,
    Columnar,
    DatabaseEntity,
    EntityModification,
    PrimaryKey,
    Table (..),
    TableEntity,
    modifyTableFields,
    setEntityName,
    tableModification,
  )
import Prelude hiding (id)

data NoteBodyT f = NoteBody
  { id :: Columnar f Int64,
    noteId :: Columnar f UUID,
    body :: Columnar f Text,
    createdAt :: Columnar f UTCTime,
    updatedAt :: Columnar f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type NoteBody = NoteBodyT Identity

instance Table NoteBodyT where
  newtype PrimaryKey NoteBodyT f = NoteBodyId
    { unNoteBodyId :: Columnar f Int64
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = NoteBodyId <<< id

noteBodySettings :: EntityModification (DatabaseEntity be db) be (TableEntity NoteBodyT)
noteBodySettings =
  setEntityName "note_bodies"
    <> modifyTableFields
      tableModification
        { id = "id",
          body = "body",
          noteId = "note_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
