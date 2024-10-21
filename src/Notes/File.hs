{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit (MonadUnliftIO, (.|))
import Conduit qualified
import Notes.DB (ManageDB)
import Notes.Hash (hashContent)
import Notes.NoteTitle.Queries (NoteTitleInput (..), insertNoteTitles)
import Notes.Parser (Note (..), parseNotes)

parseFile :: (MonadIO m, ManageDB m, MonadUnliftIO m) => FilePath -> m ()
parseFile filepath =
  Conduit.runConduitRes $
    Conduit.sourceFile filepath
      .| Conduit.mapC (parseNotes <<< decodeUtf8)
      .| Conduit.mapM_C
        ( \notes -> do
            lift $
              insertNoteTitles
                [ NoteTitleInput
                    { title = note.title,
                      hash = hashContent note.title
                    }
                  | note <- notes
                ]
        )
