{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Notes.App
  ( AppIO (..),
    AppState (..),
    runAppIO,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Lens.Micro.Platform (to)
import Notes.DB (ManageDB (..), RunDB)
import Notes.Tracing (HasTracer (..), Tracer)

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

deriving newtype instance MonadMask AppIO

deriving newtype instance MonadCatch AppIO

deriving newtype instance MonadThrow AppIO

instance ManageDB AppIO where
  dbRunner = do
    AppState {runDB} <- ask
    pure runDB

data AppState = AppState
  { runDB :: RunDB,
    tracer :: Tracer
  }

instance HasTracer AppState where
  tracer = to $ \AppState {tracer} -> tracer

runAppIO :: AppState -> AppIO a -> IO a
runAppIO appState action = runReaderT (unAppIO action) appState
