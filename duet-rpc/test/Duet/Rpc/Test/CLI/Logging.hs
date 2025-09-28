module Duet.Rpc.Test.CLI.Logging
  ( tests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , containsAnsi
  , defaultInvocation
  , runCli
  )
import Duet.Rpc.Test.CLI.Assertions (assertTextContains)
import Duet.Rpc.VersionManager (renderVersion)

tests :: TestTree
tests =
  testGroup
    "logging"
    [ testCase "--log-level debug outputs structured logs to stderr" debugLoggingToStderr
    , testCase "default log level suppresses debug output" defaultLogLevelSuppressesDebug
    , testCase "warnings surface without opting into debug" warningVisibleAtDefaultLevel
    , testCase "DUET_RPC_LOG routes logs to file" logFileRouting
    , testCase "invalid DUET_RPC_LOG falls back to stderr with warning" invalidLogPathFallback
    ]

debugLoggingToStderr :: Assertion
debugLoggingToStderr = do
  result <- runCli invocation
  cliExitCode result @?= ExitSuccess
  assertVersionStdout result
  assertTextContains (cliStderr result) "[Debug]"
  assertTextContains (cliStderr result) "duet-rpc CLI startup"
  assertTextContains (cliStderr result) "]["
  where
    invocation = defaultInvocation {cliArgs = ["--log-level", "debug", "version"]}

defaultLogLevelSuppressesDebug :: Assertion
defaultLogLevelSuppressesDebug = do
  result <- runCli invocation
  cliExitCode result @?= ExitSuccess
  assertVersionStdout result
  cliStderr result @?= ""
  where
    invocation = defaultInvocation {cliArgs = ["version"]}

warningVisibleAtDefaultLevel :: Assertion
warningVisibleAtDefaultLevel = do
  result <- runCli invocation
  cliExitCode result @?= ExitSuccess
  assertVersionStdout result

  let stderrText = cliStderr result

  assertTextContains stderrText "Warning: Cannot write to log file"
  assertTextContains stderrText "/nonexistent/x/y.log"
  T.isInfixOf "[Debug]" stderrText @?= False
  where
    invocation = defaultInvocation
      { cliArgs = ["version"]
      , cliEnv = Map.singleton "DUET_RPC_LOG" "/nonexistent/x/y.log"
      }

logFileRouting :: Assertion
logFileRouting = do
  withSystemTempFile "duet-test.log" $ \logPath logHandle -> do
    hClose logHandle

    let invocation = defaultInvocation
          { cliArgs = ["--log-level", "debug", "version"]
          , cliEnv = Map.singleton "DUET_RPC_LOG" logPath
          }

    result <- runCli invocation
    cliExitCode result @?= ExitSuccess
    assertVersionStdout result
    cliStderr result @?= ""

    logContent <- TIO.readFile logPath
    assertTextContains logContent "[Debug]"
    assertTextContains logContent "duet-rpc CLI startup"
    containsAnsi logContent @?= False

invalidLogPathFallback :: Assertion
invalidLogPathFallback = do
  result <- runCli invocation
  cliExitCode result @?= ExitSuccess
  assertVersionStdout result

  let stderrText = cliStderr result

  assertTextContains stderrText "Warning: Cannot write to log file"
  assertTextContains stderrText "/nonexistent/x/y.log"

  assertTextContains stderrText "[Debug]"
  assertTextContains stderrText "duet-rpc CLI startup"
  assertTextContains stderrText "No such file"

  let hasStackTrace = any (`T.isInfixOf` stderrText) ["Stack trace", "CallStack"]
  assertBool "Log fallback should not include stack trace" (not hasStackTrace)
  where
    invocation = defaultInvocation
      { cliArgs = ["--log-level", "debug", "version"]
      , cliEnv = Map.singleton "DUET_RPC_LOG" "/nonexistent/x/y.log"
      }

assertVersionStdout :: CliResult -> Assertion
assertVersionStdout result = cliStdout result @?= renderVersion <> "\n"
