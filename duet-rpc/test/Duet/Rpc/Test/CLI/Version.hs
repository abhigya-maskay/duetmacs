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
    , testCase "-V short flag mirrors --version" versionShortFlagMatches
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

versionShortFlagMatches :: Assertion
versionShortFlagMatches = do
  longResult <- runCli defaultInvocation {cliArgs = ["--version"]}
  shortResult <- runCli defaultInvocation {cliArgs = ["-V"]}
  cliStdout shortResult @?= cliStdout longResult
  cliStderr shortResult @?= ""
