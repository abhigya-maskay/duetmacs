module Duet.Rpc.OutputFormatter.Shell
  ( ShellFormatter (..)
  , HandleProfile (..)
  , TerminalProfile (..)
  , buildShellFormatter
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

data HandleProfile = HandleProfile
  { handleIsTTY :: Bool
  , handleSupportsAnsi :: Bool
  }

data TerminalProfile = TerminalProfile
  { stdoutProfile :: HandleProfile
  , stderrProfile :: HandleProfile
  , envNoColorEnabled :: Bool
  }

buildShellFormatter :: TerminalProfile -> CliOptions -> ShellFormatter
buildShellFormatter terminal cliOpts =
  let
    formatter =
      mkOutputFormatter
        FormatterSettings
          { stdoutColorMode = resolve (stdoutProfile terminal)
          , stderrColorMode = resolve (stderrProfile terminal)
          }
  in
    ShellFormatter
      { writeStdout = TIO.putStr . formatStdout formatter
      , writeStderr = TIO.hPutStr stderr . formatStderr formatter
      }
  where
    resolve :: HandleProfile -> ColorMode
    resolve handleProfile
      | optNoColor cliOpts
          || envNoColorEnabled terminal
          || not (handleIsTTY handleProfile)
          || not (handleSupportsAnsi handleProfile) = ColorDisabled
      | otherwise = ColorEnabled

initShellFormatter :: CliOptions -> IO ShellFormatter
initShellFormatter cliOpts = do
  envNoColor <- lookupEnv "NO_COLOR"
  stdoutAnsi <- hSupportsANSI stdout
  stderrAnsi <- hSupportsANSI stderr
  stdoutTTY <- hIsTerminalDevice stdout
  stderrTTY <- hIsTerminalDevice stderr

  let terminal =
        TerminalProfile
          { stdoutProfile = HandleProfile stdoutTTY stdoutAnsi
          , stderrProfile = HandleProfile stderrTTY stderrAnsi
          , envNoColorEnabled = isJust envNoColor
          }

  pure (buildShellFormatter terminal cliOpts)
