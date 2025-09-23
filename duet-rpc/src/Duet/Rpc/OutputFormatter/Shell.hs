module Duet.Rpc.OutputFormatter.Shell
  ( ShellFormatter (..)
  , initShellFormatter
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.IO (stdout, stderr, hIsTerminalDevice)

import System.Console.ANSI (hSupportsANSI)

import Duet.Rpc.CLI.Core (CliOptions (..))
import Duet.Rpc.OutputFormatter.Core
  ( ColorMode (..)
  , FormatterSettings (..)
  , OutputFormatter (..)
  , mkOutputFormatter
  )

data ShellFormatter = ShellFormatter
  { writeStdout :: Text -> IO ()
  , writeStderr :: Text -> IO ()
  }

initShellFormatter :: CliOptions -> IO ShellFormatter
initShellFormatter cliOpts = do
  envNoColor <- lookupEnv "NO_COLOR"
  stdoutSupportsAnsi <- hSupportsANSI stdout
  stderrSupportsAnsi <- hSupportsANSI stderr
  stdoutIsTTY <- hIsTerminalDevice stdout
  stderrIsTTY <- hIsTerminalDevice stderr
  let envNoColorEnabled = isJust envNoColor

  let formatter =
        mkOutputFormatter
          FormatterSettings
            { stdoutColorMode = resolveColorMode envNoColorEnabled stdoutIsTTY stdoutSupportsAnsi
            , stderrColorMode = resolveColorMode envNoColorEnabled stderrIsTTY stderrSupportsAnsi
            }

  pure
    ShellFormatter
      { writeStdout = TIO.putStr . formatStdout formatter
      , writeStderr = TIO.hPutStr stderr . formatStderr formatter
      }

  where
    resolveColorMode :: Bool -> Bool -> Bool -> ColorMode
    resolveColorMode envDisabled isTTY supportsAnsi
      | optNoColor cliOpts || envDisabled || not isTTY || not supportsAnsi = ColorDisabled
      | otherwise = ColorEnabled
