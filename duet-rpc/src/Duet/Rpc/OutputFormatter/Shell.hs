module Duet.Rpc.OutputFormatter.Shell
  ( ShellFormatter (..),
    initShellFormatter,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Duet.Rpc.CLI.Core (CliOptions (..))
import Duet.Rpc.OutputFormatter.Core
  ( FormatterSettings (..),
    HandleProfile (..),
    OutputFormatter (..),
    TerminalProfile (..),
    mkOutputFormatter,
    resolveFormatterSettings,
  )
import System.Console.ANSI (hSupportsANSI)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stderr, stdout)

data ShellFormatter = ShellFormatter
  { writeStdout :: Text -> IO (),
    writeStderr :: Text -> IO ()
  }

mkShellFormatter :: TerminalProfile -> CliOptions -> ShellFormatter
mkShellFormatter terminal cliOpts =
  let settings :: FormatterSettings
      settings = resolveFormatterSettings cliOpts terminal

      formatter :: OutputFormatter
      formatter = mkOutputFormatter settings
   in ShellFormatter
        { writeStdout = TIO.putStr . formatStdout formatter,
          writeStderr = TIO.hPutStr stderr . formatStderr formatter
        }

initShellFormatter :: CliOptions -> IO ShellFormatter
initShellFormatter cliOpts = do
  envNoColor <- lookupEnv "NO_COLOR"
  stdoutAnsi <- hSupportsANSI stdout
  stderrAnsi <- hSupportsANSI stderr
  stdoutTTY <- hIsTerminalDevice stdout
  stderrTTY <- hIsTerminalDevice stderr

  let terminal =
        TerminalProfile
          { stdoutProfile = HandleProfile stdoutTTY stdoutAnsi,
            stderrProfile = HandleProfile stderrTTY stderrAnsi,
            envNoColorEnabled = isJust envNoColor
          }

  pure (mkShellFormatter terminal cliOpts)
