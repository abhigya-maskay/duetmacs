module Duet.Rpc.Test.CLI.Logging
  ( tests
  ) where

import Control.Monad (forM_, when)
import Data.Char (isUpper)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (LocalTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

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
  assertStructuredLogPrefix (cliStderr result)
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
    assertStructuredLogPrefix logContent

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
  assertStructuredLogPrefix stderrText
  where
    invocation = defaultInvocation
      { cliArgs = ["--log-level", "debug", "version"]
      , cliEnv = Map.singleton "DUET_RPC_LOG" "/nonexistent/x/y.log"
      }

assertVersionStdout :: CliResult -> Assertion
assertVersionStdout result = cliStdout result @?= renderVersion <> "\n"

assertStructuredLogPrefix :: T.Text -> Assertion
assertStructuredLogPrefix rawText =
  case structuredLines of
    [] -> assertFailure "Expected structured log lines with level prefixes, but none were found."
    _ -> forM_ (zip [1 :: Int ..] structuredLines) $ \(idx, line) ->
      case validateStructuredPrefix line of
        Left err ->
          assertFailure $ "Log line " <> show idx <> " missing structured prefix: " <> err <> " (" <> T.unpack line <> ")"
        Right () -> pure ()
  where
    structuredLines = filter startsWithBracket (nonEmptyLines rawText)

    nonEmptyLines :: T.Text -> [T.Text]
    nonEmptyLines = filter (not . T.null) . T.lines . normalizeNewlines

    normalizeNewlines :: T.Text -> T.Text
    normalizeNewlines = T.replace "\r\n" "\n" . T.replace "\r" "\n"

    startsWithBracket :: T.Text -> Bool
    startsWithBracket = (== Just '[') . fmap fst . T.uncons . T.stripStart

validateStructuredPrefix :: T.Text -> Either String ()
validateStructuredPrefix line = do
  let cleaned = T.stripStart line
  (timestampText, restAfterTimestamp) <- bracketSegment cleaned "timestamp"
  parseTimestamp timestampText

  (_, restAfterNamespace) <- bracketSegment restAfterTimestamp "namespace"
  (levelText, _restAfterLevel) <- bracketSegment restAfterNamespace "level"
  when (T.null levelText) $ Left "empty level name"
  when (not (startsWithUpper levelText)) $ Left "level must start with uppercase"
  Right ()

parseTimestamp :: T.Text -> Either String ()
parseTimestamp t =
  case iso8601ParseM (T.unpack t) :: Maybe UTCTime of
    Just _ -> Right ()
    Nothing ->
      case parseTimeM True defaultTimeLocale "%F %T" (T.unpack t) :: Maybe LocalTime of
        Just _ -> Right ()
        Nothing -> Left "timestamp not ISO-8601"

startsWithUpper :: T.Text -> Bool
startsWithUpper txt =
  case T.uncons txt of
    Nothing -> False
    Just (c, _) -> isUpper c

bracketSegment :: T.Text -> String -> Either String (T.Text, T.Text)
bracketSegment input label = do
  rest1 <- maybe (Left $ "missing [ at beginning of " <> label <> " segment") Right (T.stripPrefix "[" input)
  let (segment, remainder) = T.breakOn "]" rest1
  remainderStripped <- maybe (Left $ "missing closing ] for " <> label <> " segment") Right (T.stripPrefix "]" remainder)
  pure (segment, remainderStripped)
