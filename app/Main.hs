{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml (decodeFileEither)
import Notes.App (AppState (..), runAppIO)
import Notes.Config (Configuration (..), initConnectionPool)
import Notes.DB (newRunDB)
import Notes.File (parseFile)
import Notes.Tracing
  ( TraceSampling (..),
    runTracer,
    spanOpts,
    traced_,
    withRootTracer,
  )

main :: IO ()
main = do
  config <-
    fmap (either (error <<< show) identity) $
      decodeFileEither @Configuration "config.yaml"

  connectionPool <- initConnectionPool config.connectionString

  let runDB = newRunDB connectionPool

  let traceSampling =
        TraceSampling
          { stdioSamplingRate = Nothing,
            samplingRate = Nothing,
            enableStdioReporter = True
          }

  withRootTracer traceSampling $ \tracer -> runTracer tracer $ do
    traced_ (spanOpts "parse-notes-init" mempty) $ \span -> do
      let appState =
            AppState
              { runDB,
                tracer
              }

      liftIO $ runAppIO appState $ do
        parseFile span "./test/sample-file/TestFile.hs"
