module Duet.Rpc.Test.CLI.Errors
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import System.Exit (ExitCode (..))

import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , defaultInvocation
  , runCli
  )
import Duet.Rpc.Test.CLI.Assertions (assertTextContains)

tests :: TestTree
tests =
  testGroup
    "errors"
    [ testCase "unknown subcommand returns usage on stderr" unknownSubcommand
    ]

unknownSubcommand :: Assertion
unknownSubcommand = do
  result <- runCli defaultInvocation {cliArgs = ["frobnicate"]}
  cliExitCode result @?= ExitFailure 1
  cliStdout result @?= ""
  assertContainsUsage result

assertContainsUsage :: CliResult -> Assertion
assertContainsUsage result =
  assertTextContains (cliStderr result) "Usage:"
