{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit ((.|))
import Conduit qualified
import Data.UUID.V4 qualified
import Notes.DB (ManageDB)
import Notes.Parser qualified
-- import Notes.NoteTitle.Queries (NoteTitleInput (..), insertNoteTitles)
import Notes.Tracing
  ( ActiveSpan,
    MonadTracer,
    childOf,
    spanOpts,
    traced_,
  )

parseFile ::
  (ManageDB m, MonadTracer r m) =>
  ActiveSpan ->
  FilePath ->
  m ()
parseFile span filepath =
  traced_ (spanOpts "parse-file" $ childOf span) $ \_span ->
    Conduit.runConduitRes $
      Conduit.sourceFile filepath
        -- .| Conduit.mapC (Notes.Parser.parseFile <<< decodeUtf8)
        .| Conduit.mapM_C
          ( \fileContent -> do
              -- make it async when rendering back to file and saving to DB.
              notes <-
                Notes.Parser.parseFileM
                  span
                  Notes.Parser.GenerateBodyId
                    { generate = \_ -> liftIO Data.UUID.V4.nextRandom
                    }
                  (decodeUtf8 fileContent)
              print notes
              -- lift $
              --   insertNoteTitles
              --     span
              --     [ NoteTitleInput
              --         { title = toText note.title,
              --           hash = toText note.title
              --         }
              --       | note <- notes
              --     ]
          )
