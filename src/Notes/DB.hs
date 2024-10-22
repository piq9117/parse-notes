{-# LANGUAGE OverloadedStrings #-}

module Notes.DB
  ( ManageDB (..),
    newRunDB,
    RunDB (..),
  )
where

import Data.Pool (Pool, withResource)
import Database.Beam.Sqlite (SqliteM, runBeamSqliteDebug)
import Database.SQLite.Simple (Connection, withTransaction)
import Notes.Tracing (ActiveSpan, LogField (..), addLogRecord)

type Query m = (m ~ SqliteM)

newtype RunDB = RunDB
  { runDB' :: forall m n a. (MonadIO m, Query n) => ActiveSpan -> n a -> m a
  }

class ManageDB m where
  dbRunner :: m RunDB
  runQuery :: (MonadIO m, Query n) => ActiveSpan -> n a -> m a
  runQuery span query = do
    db <- dbRunner
    runDB db span query

runDB :: (MonadIO m, Query n) => RunDB -> ActiveSpan -> n a -> m a
runDB RunDB {runDB'} query span = runDB' query span

newRunDB :: Pool Connection -> RunDB
newRunDB pool =
  RunDB
    { runDB' = \span query ->
        liftIO $ withResource pool $ \connection -> do
          let eval =
                runBeamSqliteDebug
                  ( \statements ->
                      addLogRecord span (LogField "query" statements)
                  )
                  connection
                  query

          withTransaction connection eval
    }
