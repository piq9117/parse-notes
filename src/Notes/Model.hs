{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Notes.Model (database, Database (..)) where

import Database.Beam
  ( DatabaseSettings,
    TableEntity,
    dbModification,
    defaultDbSettings,
    withDbModification,
  )
import Database.Beam qualified
import Notes.Models.NoteBody qualified
import Notes.Models.NoteTitle qualified

data Database f = Database
  { noteTitles :: f (TableEntity Notes.Models.NoteTitle.NoteTitleT),
    noteBodies :: f (TableEntity Notes.Models.NoteBody.NoteBodyT)
  }
  deriving stock (Generic)
  deriving anyclass (Database.Beam.Database backend)

database :: DatabaseSettings backend Database
database =
  defaultDbSettings
    `withDbModification` dbModification
      { noteTitles = Notes.Models.NoteTitle.noteTitleSettings,
        noteBodies = Notes.Models.NoteBody.noteBodySettings
      }
