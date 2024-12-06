{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Config
  ( loadConfig,
    ServerConfig (..),
    AppConfig (..),
    ConfigError (..),
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Yaml (FromJSON, decodeFileEither)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

data ServerConfig = ServerConfig
  { port :: Int
  }
  deriving (Show, Generic, FromJSON)

data AppConfig = AppConfig
  { server :: ServerConfig
  }
  deriving (Show, Generic, FromJSON)

data ConfigError
  = ConfigFileNotFound FilePath
  | ConfigParseError String
  deriving (Show, Eq)

loadConfig :: FilePath -> ExceptT ConfigError IO AppConfig
loadConfig filePath = do
  fileExists <- liftIO $ doesFileExist filePath
  when (not fileExists) $ throwError $ ConfigFileNotFound filePath
  result <- liftIO $ decodeFileEither filePath
  case result of
    Left err -> throwError $ ConfigParseError (show err)
    Right config -> return config
