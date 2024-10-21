{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Yaml (decodeFileEither)
import Notes.App (AppState (..), runAppIO)
import Notes.Config (Configuration (..), initConnectionPool)
import Notes.DB (newRunDB)
import Notes.File (parseFile)

main :: IO ()
main = do
  config <-
    fmap (either (error <<< show) identity) $
      decodeFileEither @Configuration "config.yaml"

  connectionPool <- initConnectionPool config.connectionString

  let runDB = newRunDB connectionPool
  let appState =
        AppState
          { runDB
          }

  runAppIO appState $ do
    parseFile "./test/sample-file/TestFile.hs"
