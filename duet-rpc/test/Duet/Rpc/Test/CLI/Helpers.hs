module Duet.Rpc.Test.CLI.Helpers
  ( assertTextContains,
    parseCli,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Duet.Rpc.CLI.Core (CliOptions, cliParserInfo, prefsWithHelp)
import Options.Applicative (ParserResult)
import qualified Options.Applicative as OA
import Test.Tasty.HUnit (Assertion, (@?=))

assertTextContains :: Text -> Text -> Assertion
assertTextContains haystack needle = T.isInfixOf needle haystack @?= True

parseCli :: [String] -> ParserResult CliOptions
parseCli = OA.execParserPure prefsWithHelp cliParserInfo
