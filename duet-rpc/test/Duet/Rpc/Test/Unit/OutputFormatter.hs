module Duet.Rpc.Test.Unit.OutputFormatter
  ( tests,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Duet.Rpc.CLI.Core (CliOptions (..), defaultCliOptions)
import Duet.Rpc.OutputFormatter.Core
  ( ColorMode (..),
    FormatterSettings (..),
    HandleProfile (..),
    OutputFormatter (..),
    TerminalProfile (..),
    mkOutputFormatter,
    resolveFormatterSettings,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "OutputFormatter"
    [ testCase "enabled color wraps payload with ANSI and newline" colorEnabledWrapsAnsi,
      testCase "disabled color strips existing ANSI sequences" colorDisabledStripsAnsi,
      testCase "formatter always terminates output with newline" enforcesTrailingNewline,
      testCase "--no-color overrides terminal capabilities" noColorFlagOverridesTerminal,
      testCase "NO_COLOR environment disables color" envNoColorOverridesTerminal,
      testCase "non-TTY stdout disables color" nonTtyDisablesColor,
      testCase "lack of ANSI support disables color" ansiUnsupportedDisablesColor,
      testCase "TTY with ANSI and defaults keeps color" ttyAnsiDefaultsEnableColor
    ]

colorEnabledWrapsAnsi :: Assertion
colorEnabledWrapsAnsi = do
  let formatted = formatStdout (formatter ColorEnabled) payload
  assertBool "formatted text should contain ANSI escape" (containsAnsi formatted)
  assertBool "formatted text should end with newline" (T.isSuffixOf "\n" formatted)
  where
    payload :: Text
    payload = "hello"

colorDisabledStripsAnsi :: Assertion
colorDisabledStripsAnsi =
  formatStdout (formatter ColorDisabled) payload @?= "hello\n"
  where
    payload :: Text
    payload = "\ESC[31mhello\ESC[0m"

enforcesTrailingNewline :: Assertion
enforcesTrailingNewline =
  formatStdout (formatter ColorDisabled) "hello" @?= "hello\n"

noColorFlagOverridesTerminal :: Assertion
noColorFlagOverridesTerminal =
  stdoutColorMode settings @?= ColorDisabled
  where
    opts :: CliOptions
    opts = defaultCliOptions {optNoColor = True}

    settings :: FormatterSettings
    settings = resolveFormatterSettings opts ttyAnsiTerminal

envNoColorOverridesTerminal :: Assertion
envNoColorOverridesTerminal =
  stdoutColorMode settings @?= ColorDisabled
  where
    terminal :: TerminalProfile
    terminal = ttyAnsiTerminal {envNoColorEnabled = True}

    settings :: FormatterSettings
    settings = resolveFormatterSettings defaultCliOptions terminal

nonTtyDisablesColor :: Assertion
nonTtyDisablesColor =
  stdoutColorMode settings @?= ColorDisabled
  where
    terminal :: TerminalProfile
    terminal = ttyAnsiTerminal {stdoutProfile = HandleProfile False True}

    settings :: FormatterSettings
    settings = resolveFormatterSettings defaultCliOptions terminal

ansiUnsupportedDisablesColor :: Assertion
ansiUnsupportedDisablesColor =
  stdoutColorMode settings @?= ColorDisabled
  where
    terminal :: TerminalProfile
    terminal = ttyAnsiTerminal {stdoutProfile = HandleProfile True False}

    settings :: FormatterSettings
    settings = resolveFormatterSettings defaultCliOptions terminal

ttyAnsiDefaultsEnableColor :: Assertion
ttyAnsiDefaultsEnableColor =
  stdoutColorMode settings @?= ColorEnabled
  where
    settings :: FormatterSettings
    settings = resolveFormatterSettings defaultCliOptions ttyAnsiTerminal

formatter :: ColorMode -> OutputFormatter
formatter mode =
  mkOutputFormatter FormatterSettings {stdoutColorMode = mode, stderrColorMode = mode}

ttyAnsiTerminal :: TerminalProfile
ttyAnsiTerminal =
  TerminalProfile
    { stdoutProfile = HandleProfile True True,
      stderrProfile = HandleProfile True True,
      envNoColorEnabled = False
    }

containsAnsi :: Text -> Bool
containsAnsi = T.any (== '\ESC')
