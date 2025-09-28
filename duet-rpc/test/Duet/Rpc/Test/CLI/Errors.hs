module Duet.Rpc.Test.CLI.Errors
  ( tests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import System.Exit (ExitCode (..))

import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , containsAnsi
  , defaultInvocation
  , runCli
  , runCliViaScript
  )
import Duet.Rpc.Test.CLI.Assertions (assertTextContains)

tests :: TestTree
tests =
  testGroup
    "errors"
    [ testCase "unknown subcommand returns usage on stderr" unknownSubcommand
    , testCase "TTY failure honors NO_COLOR" unknownSubcommandNoColorViaTTY
    ]

unknownSubcommand :: Assertion
unknownSubcommand = do
  result <- runCli defaultInvocation {cliArgs = ["frobnicate"], cliExpectSuccess = False}
  cliExitCode result @?= ExitFailure 1
  cliStdout result @?= ""
  assertContainsUsage result
  assertNoStackTrace (cliStderr result)

assertContainsUsage :: CliResult -> Assertion
assertContainsUsage result =
  assertTextContains (cliStderr result) "Usage:"

unknownSubcommandNoColorViaTTY :: Assertion
unknownSubcommandNoColorViaTTY = do
  baseline <- runCliViaScript defaultInvocation {cliArgs = ["frobnicate"], cliExpectSuccess = False}
  case baseline of
    Left reason ->
      assertBool (T.unpack ("Skipping TTY test: " <> reason)) True
    Right _ -> do
      envResult <-
        runCliViaScript
          defaultInvocation
            { cliArgs = ["frobnicate"]
            , cliEnv = Map.singleton "NO_COLOR" "1"
            , cliExpectSuccess = False
            }
      case envResult of
        Left reason ->
          assertBool (T.unpack ("Skipping TTY test: " <> reason)) True
        Right plain -> do
          let combined = cliStdout plain <> cliStderr plain
          containsAnsi combined @?= False
          assertTextContains combined "Usage:"
          assertNoStackTrace (cliStderr plain)

assertNoStackTrace :: T.Text -> Assertion
assertNoStackTrace stderrText = mapM_ forbid disallowedMarkers
  where
    forbid :: T.Text -> Assertion
    forbid marker =
      assertBool
        "CLI error output should not include stack traces"
        (not (marker `T.isInfixOf` stderrText))

    disallowedMarkers :: [T.Text]
    disallowedMarkers = ["CallStack", "Exception:"]
