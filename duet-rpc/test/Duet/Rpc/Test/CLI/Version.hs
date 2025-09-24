module Duet.Rpc.Test.CLI.Version
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , defaultInvocation
  , runCli
  )
import Duet.Rpc.VersionManager (renderVersion)

tests :: TestTree
tests =
  testGroup
    "version"
    [ testCase "--version prints semver" versionGlobalFlag
    , testCase "version subcommand matches --version" versionSubcommandMatchesFlag
    ]

versionGlobalFlag :: Assertion
versionGlobalFlag = do
  result <- runCli invocation
  cliStdout result @?= renderVersion <> "\n"
  cliStderr result @?= ""
  where
    invocation = defaultInvocation {cliArgs = ["--version"]}

versionSubcommandMatchesFlag :: Assertion
versionSubcommandMatchesFlag = do
  flagResult <- runCli defaultInvocation {cliArgs = ["--version"]}
  subResult <- runCli defaultInvocation {cliArgs = ["version"]}
  cliStdout subResult @?= cliStdout flagResult
  cliStderr subResult @?= ""
