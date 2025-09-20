module Main where

import Control.Applicative (some)
import Options.Applicative (ParserInfo, ReadM, argument, customExecParser, fullDesc, helper, info, metavar, prefs, readerError, showHelpOnEmpty, showHelpOnError, str, (<**>))

main :: IO ()
main = customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) opts

opts :: ParserInfo ()
opts = info ((() <$ some (argument commandArg (metavar "COMMAND"))) <**> helper) fullDesc
  where
    commandArg :: ReadM String
    commandArg = do
      cmd <- str
      readerError ("Invalid command: " ++ cmd)
