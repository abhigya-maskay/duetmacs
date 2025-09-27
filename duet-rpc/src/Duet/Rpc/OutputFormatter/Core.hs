module Duet.Rpc.OutputFormatter.Core
  ( ColorMode (..)
  , FormatterSettings (..)
  , OutputFormatter (..)
  , HandleProfile (..)
  , TerminalProfile (..)
  , resolveFormatterSettings
  , mkOutputFormatter
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI
  ( Color (White)
  , ColorIntensity (Vivid)
  , ConsoleIntensity (BoldIntensity)
  , ConsoleLayer (Foreground)
  , SGR (Reset, SetColor, SetConsoleIntensity)
  , setSGRCode
  )

import Duet.Rpc.CLI.Core (CliOptions (..))

data ColorMode
  = ColorEnabled
  | ColorDisabled
  deriving (Eq, Show)

data FormatterSettings = FormatterSettings
  { stdoutColorMode :: ColorMode
  , stderrColorMode :: ColorMode
  }
  deriving (Eq, Show)

data OutputFormatter = OutputFormatter
  { formatStdout :: Text -> Text
  , formatStderr :: Text -> Text
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

resolveFormatterSettings :: CliOptions -> TerminalProfile -> FormatterSettings
resolveFormatterSettings cliOpts terminal =
  FormatterSettings
    { stdoutColorMode = resolve (stdoutProfile terminal)
    , stderrColorMode = resolve (stderrProfile terminal)
    }
  where
    resolve :: HandleProfile -> ColorMode
    resolve handleProfile
      | optNoColor cliOpts
          || envNoColorEnabled terminal
          || not (handleIsTTY handleProfile)
          || not (handleSupportsAnsi handleProfile) = ColorDisabled
      | otherwise = ColorEnabled

mkOutputFormatter :: FormatterSettings -> OutputFormatter
mkOutputFormatter settings =
  OutputFormatter
    { formatStdout = formatWith (stdoutColorMode settings)
    , formatStderr = formatWith (stderrColorMode settings)
    }
  where
    formatWith :: ColorMode -> Text -> Text
    formatWith mode = ensureTrailingNewline . applyColorMode mode

ensureTrailingNewline :: Text -> Text
ensureTrailingNewline txt
  | T.null txt = T.singleton '\n'
  | T.isSuffixOf "\n" txt = txt
  | otherwise = T.snoc txt '\n'

applyColorMode :: ColorMode -> Text -> Text
applyColorMode ColorDisabled = stripAnsi
applyColorMode ColorEnabled = applyAnsi

applyAnsi :: Text -> Text
applyAnsi txt
  | T.null txt = txt
  | otherwise = ansiPrefix <> txt <> ansiSuffix

ansiPrefix :: Text
ansiPrefix = renderSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]

ansiSuffix :: Text
ansiSuffix = renderSGR [Reset]

renderSGR :: [SGR] -> Text
renderSGR = T.pack . setSGRCode

ansiEscape :: Text
ansiEscape = "\ESC"

stripAnsi :: Text -> Text
stripAnsi txt =
  case T.breakOn ansiEscape txt of
    (prefix, rest)
      | T.null rest -> prefix
      | otherwise ->
          let afterEsc = T.drop 1 rest
           in prefix <> stripRemaining afterEsc
  where
    stripRemaining t =
      case T.uncons t of
        Nothing -> T.empty
        Just ('[', tailTxt) -> stripUntilEnd tailTxt
        Just (_, tailTxt) -> stripAnsi tailTxt

    stripUntilEnd t =
      case T.uncons t of
        Nothing -> T.empty
        Just (c, tailTxt)
          | isAnsiTerminator c -> stripAnsi tailTxt
          | otherwise -> stripUntilEnd tailTxt

    isAnsiTerminator :: Char -> Bool
    isAnsiTerminator c = c >= '@' && c <= '~'
