module Duet.Rpc.Config.Loader
  ( ConfigSource (..)
  , Config (..)
  , configEnvVar
  , projectConfigFileName
  , userConfigFilePath
  , configSearchOrder
  , searchPaths
  , defaultConfig
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Duet.Rpc.CLI.Core (LogLevel)

data ConfigSource
  = SourceFlags
  | SourceEnv
  | SourceProject
  | SourceUser
  | SourceDefault
  deriving (Eq, Ord, Enum, Bounded, Show)

configSearchOrder :: [ConfigSource]
configSearchOrder = [minBound .. maxBound]

configEnvVar :: Text
configEnvVar = "DUET_RPC_CONFIG"

projectConfigFileName :: FilePath
projectConfigFileName = ".duet-rpc.toml"

userConfigFilePath :: FilePath
userConfigFilePath = "~/.config/duet-rpc/config.toml"

searchPaths :: [(ConfigSource, FilePath)]
searchPaths = mapMaybe pathFor configSearchOrder
  where
    pathFor SourceProject = Just (SourceProject, projectConfigFileName)
    pathFor SourceUser = Just (SourceUser, userConfigFilePath)
    pathFor _ = Nothing

data Config = Config
  { configPath :: Maybe FilePath
  , configSource :: ConfigSource
  , logLevelOverride :: Maybe LogLevel
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { configPath = Nothing
    , configSource = SourceDefault
    , logLevelOverride = Nothing
    }
