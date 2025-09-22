{-# LANGUAGE OverloadedStrings #-}

module Duet.Rpc.OutputFormatter.Core
  ( ColorMode (..)
  , FormatterSettings (..)
  , OutputFormatter (..)
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
ensureTrailingNewline txt =
  case T.unsnoc txt of
    Nothing -> T.singleton '\n'
    Just (_, '\n') -> txt
    Just _ -> T.snoc txt '\n'

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

stripAnsi :: Text -> Text
stripAnsi txt =
  case T.breakOn "\ESC" txt of
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
          | c >= '@' && c <= '~' -> stripAnsi tailTxt
          | otherwise -> stripUntilEnd tailTxt
