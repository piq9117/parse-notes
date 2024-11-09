{-# OPTIONS_GHC -Wno-orphans #-}

module Notes.Models.Instances where

import Data.UUID (UUID)
import Data.UUID qualified
import Database.Beam.Backend (HasSqlValueSyntax (..))
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax (..))

instance HasSqlValueSyntax SqliteValueSyntax UUID where
  sqlValueSyntax uuid = sqlValueSyntax @SqliteValueSyntax @Text (Data.UUID.toText uuid)
