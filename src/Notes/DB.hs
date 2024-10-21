module Notes.DB
  ( ManageDB (..),
    newRunDB,
    RunDB (..),
  )
where

import Data.Pool (Pool, withResource)
import Database.Beam.Sqlite (SqliteM, runBeamSqlite)
import Database.SQLite.Simple (Connection)

type Query m = (m ~ SqliteM)

newtype RunDB = RunDB
  { runDB' :: forall m n a. (MonadIO m, Query n) => n a -> m a
  }

class ManageDB m where
  dbRunner :: m RunDB
  runQuery :: (MonadIO m, Query n) => n a -> m a
  runQuery query = do
    db <- dbRunner
    runDB db query

runDB :: (MonadIO m, Query n) => RunDB -> n a -> m a
runDB RunDB {runDB'} query = runDB' query

newRunDB :: Pool Connection -> RunDB
newRunDB pool =
  RunDB
    { runDB' = \query ->
        liftIO $ withResource pool $ \connection -> do
          runBeamSqlite connection query
    }
