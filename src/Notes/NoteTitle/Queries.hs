{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.NoteTitle.Queries
  ( insertFileContent,
    deleteFileContent,
    getFileContent,
    insertNotes,
  )
where

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
import Notes.Models.NoteBody (NoteBodyT (..))
import Notes.Models.NoteTitle (NoteTitleT (..))
import Notes.Parser qualified
import Notes.Render qualified
import Notes.Tracing
  ( ActiveSpan,
    MonadTracer,
    childOf,
    spanOpts,
    traced_,
  )
import Prelude hiding (id)

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

insertNotes ::
  (MonadTracer r m, ManageDB m) =>
  ActiveSpan ->
  [Notes.Parser.Note] ->
  m ()
insertNotes span notes =
  traced_ (spanOpts "insert-notes" $ childOf span) $ \span ->
    runQuery span $ do
      runInsert $
        insertOnConflict
          database.noteTitles
          ( insertExpressions $
              [ NoteTitle
                  { id = default_,
                    title = val_ ((\(Notes.Parser.NoteTitle title) -> title) $ note.title),
                    noteId = val_ ((\(Notes.Parser.NoteId noteId) -> noteId) $ note.id),
                    createdAt = default_,
                    updatedAt = default_
                  }
                | note <- notes
              ]
          )
          (conflictingFields $ \note -> note.noteId)
          ( onConflictUpdateSet $ \old new ->
              mconcat
                [ old.title <-. new.title,
                  old.updatedAt <-. new.updatedAt
                ]
          )

      runInsert $
        insertOnConflict
          database.noteBodies
          ( insertExpressions $ do
              note <- notes
              [ NoteBody
                  { id = default_,
                    noteId = val_ ((\(Notes.Parser.NoteId noteId) -> noteId) $ note.id),
                    body = val_ (toText $ Notes.Render.render $ Notes.Render.prettyPrint note.body),
                    createdAt = default_,
                    updatedAt = default_
                  }
                ]
          )
          (conflictingFields $ \note -> note.noteId)
          ( onConflictUpdateSet $ \old new ->
              mconcat
                [ old.body <-. new.body,
                  old.updatedAt <-. new.updatedAt
                ]
          )
