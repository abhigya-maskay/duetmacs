module Duet.Rpc.Test.Unit.ConfigLoader
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Duet.Rpc.Config.Loader
  ( Config (..),
    ConfigSource (..),
    configEnvVar,
    configSearchOrder,
    defaultConfig,
    projectConfigFileName,
    searchPaths,
    userConfigFilePath,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

allConfigSources :: [ConfigSource]
allConfigSources = [minBound .. maxBound]

allPaths :: [(ConfigSource, FilePath)]
allPaths =
  [ (SourceProject, projectConfigFileName),
    (SourceUser, userConfigFilePath)
  ]

allStringSettings :: [(Text, FilePath)]
allStringSettings =
  [ ("env var", T.unpack configEnvVar),
    ("project file", projectConfigFileName),
    ("user file", userConfigFilePath)
  ]

configSearchOrderCoversEverySource :: Assertion
configSearchOrderCoversEverySource =
  configSearchOrder @?= allConfigSources

searchPathsMatchDocumentedPrecedence :: Assertion
searchPathsMatchDocumentedPrecedence =
  searchPaths @?= allPaths

configSkeletonStringConstantsAreStable :: Assertion
configSkeletonStringConstantsAreStable =
  traverse_ assertSetting allStringSettings
  where
    assertSetting (label, actual) =
      actual @?= expected label

    expected "env var" = "DUET_RPC_CONFIG"
    expected "project file" = ".duet-rpc.toml"
    expected "user file" = "~/.config/duet-rpc/config.toml"
    expected other = error ("unexpected label: " <> show other)

defaultConfigMatchesStorySkeleton :: Assertion
defaultConfigMatchesStorySkeleton = do
  configPath defaultConfig @?= Nothing
  configSource defaultConfig @?= SourceDefault
  logLevelOverride defaultConfig @?= Nothing

tests :: TestTree
tests =
  testGroup
    "ConfigLoader"
    [ testCase "configSearchOrder covers every source" configSearchOrderCoversEverySource,
      testCase "searchPaths matches documented precedence" searchPathsMatchDocumentedPrecedence,
      testCase "config skeleton string constants are stable" configSkeletonStringConstantsAreStable,
      testCase "defaultConfig matches Story 001 skeleton" defaultConfigMatchesStorySkeleton
    ]
