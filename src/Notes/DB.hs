module Notes.DB
  ( ManageDB (..),
    newRunDB,
    RunDB (..),
  )
where

import Data.Pool (Pool, withResource)
import Database.Beam.Sqlite (SqliteM, runBeamSqlite)
import Database.SQLite.Simple (Connection)
import Notes.Tracing (ActiveSpan)

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
    { runDB' = \_span query ->
        liftIO $ withResource pool $ \connection -> do
          runBeamSqlite connection query
    }
