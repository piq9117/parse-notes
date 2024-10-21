{-# LANGUAGE DerivingStrategies #-}

module Notes.App
  ( AppIO (..),
    AppState (..),
    runAppIO,
  )
where

import Conduit (MonadUnliftIO)
import Notes.DB (ManageDB (..), RunDB)

newtype AppIO a = AppIO
  { unAppIO :: ReaderT AppState IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppState
    )

deriving newtype instance MonadUnliftIO AppIO

instance ManageDB AppIO where
  dbRunner = do
    AppState {runDB} <- ask
    pure runDB

data AppState = AppState
  { runDB :: RunDB
  }

runAppIO :: AppState -> AppIO a -> IO a
runAppIO appState action = runReaderT (unAppIO action) appState
