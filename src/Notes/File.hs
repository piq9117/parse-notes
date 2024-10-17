{-# LANGUAGE OverloadedStrings #-}

module Notes.File (parseFile) where

import Conduit ((.|))
import Conduit qualified
import Crypto.Hash.MD5 (hash)
import Notes.Parser (parseNotes)

parseFile :: FilePath -> IO ()
parseFile filepath =
  Conduit.runConduitRes $
    Conduit.sourceFile filepath
      .| Conduit.mapC (parseNotes <<< decodeUtf8)
      .| Conduit.concatMapC identity
      .| Conduit.mapM_C
        ( \notes ->
            print $ hash (show @ByteString notes)
        )
