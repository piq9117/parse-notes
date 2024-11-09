{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Notes.Models.FileContentCache
  ( FileContentCacheT (..),
    FileContentCache,
    FileContentCacheId,
    PrimaryKey (..),
    fileContentCacheSettings,
  )
where

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

data FileContentCacheT f = FileContentCache
  { id :: Columnar f Int32,
    content :: Columnar f ByteString
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type FileContentCache = FileContentCacheT Identity

type FileContentCacheId = PrimaryKey FileContentCacheT Identity

instance Table FileContentCacheT where
  newtype PrimaryKey FileContentCacheT f = FileContentId
    { unFileContentId :: Columnar f Int32
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey = FileContentId <<< id

fileContentCacheSettings :: EntityModification (DatabaseEntity be db) be (TableEntity FileContentCacheT)
fileContentCacheSettings =
  setEntityName "file_contents_cache"
    <> modifyTableFields
      tableModification
        { id = "id",
          content = "content"
        }
