{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Notes.Config
  ( Configuration (..),
    initConnectionPool,
  )
where

import Control.Concurrent (getNumCapabilities)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.SQLite.Simple (Connection, close, open)

data Configuration = Configuration
  { connectionString :: !Text
  }

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \c ->
    Configuration <$> c .: "connection_string"

initConnectionPool :: Text -> IO (Pool Connection)
initConnectionPool connectionString = do
  maxResources <- getNumCapabilities
  newPool $ defaultPoolConfig (open $ toString connectionString) close 60 (2 * maxResources)
