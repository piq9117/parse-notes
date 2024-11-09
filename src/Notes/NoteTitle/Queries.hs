{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.NoteTitle.Queries
  ( NoteTitleInput (..),
    insertNoteTitles,
    insertFileContent,
    deleteFileContent,
    getFileContent,
  )
where

import Data.UUID (UUID)
import Database.Beam
  ( all_,
    default_,
    delete,
    desc_,
    insert,
    insertExpressions,
    limit_,
    orderBy_,
    runDelete,
    runInsert,
    runSelectReturningList,
    select,
    val_,
    (<-.),
  )
import Database.Beam.Backend.SQL.BeamExtensions
  ( conflictingFields,
    insertOnConflict,
    onConflictUpdateSet,
  )
import Notes.DB (ManageDB (..))
import Notes.Model (Database (..), database)
import Notes.Models.FileContentCache (FileContentCacheT (..))
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

insertFileContent ::
  (MonadTracer r m, ManageDB m) =>
  ActiveSpan ->
  ByteString ->
  m ()
insertFileContent span content =
  traced_ (spanOpts "insert-file-content" $ childOf span) $ \span ->
    runQuery span $
      runInsert $
        insert
          database.fileContentCache
          ( insertExpressions
              [ Notes.Models.FileContentCache.FileContentCache
                  { id = default_,
                    content = val_ content
                  }
              ]
          )

getFileContent ::
  (MonadTracer r m, ManageDB m) =>
  ActiveSpan ->
  m [ByteString]
getFileContent span =
  traced_ (spanOpts "get-file-content" $ childOf span) $ \span -> do
    result <- runQuery span $
      runSelectReturningList $
        select $
          limit_ 1 $
            orderBy_ (\fileContent -> desc_ fileContent.id) $ do
              all_ database.fileContentCache

    pure $ fmap (.content) result

deleteFileContent ::
  (MonadTracer r m, ManageDB m) =>
  ActiveSpan ->
  m ()
deleteFileContent span =
  traced_ (spanOpts "delete-file-content" $ childOf span) $ \span ->
    runQuery span $
      runDelete $
        delete database.fileContentCache (const $ val_ True)

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
                    noteId = val_ noteTitle.noteId,
                    createdAt = default_,
                    updatedAt = default_
                  }
                | noteTitle <- noteTitles
              ]
          )
          (conflictingFields $ \asyncExport -> asyncExport.noteId)
          ( onConflictUpdateSet $ \old new ->
              mconcat
                [ old.title <-. new.title,
                  old.updatedAt <-. new.updatedAt
                ]
          )
