{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit qualified
import Notes.Parser (parseNotes)

parseFile :: FilePath -> IO ()
parseFile filepath =
  Conduit.runConduitRes $
    Conduit.sourceFile filepath
      Conduit..| Conduit.mapMC
        ( \content -> do
            print content
            print $ parseNotes (decodeUtf8 content)
        )
      Conduit..| Conduit.sinkNull
