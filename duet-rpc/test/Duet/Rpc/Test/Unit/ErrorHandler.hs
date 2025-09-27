module Duet.Rpc.Test.Unit.ErrorHandler
  ( tests
  ) where

import qualified Data.Text as T
import Options.Applicative (ParserResult (..))
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

import Duet.Rpc.CLI.Core (CliOptions (..), parseCli)

tests :: TestTree
tests =
  testGroup
    "ErrorHandler"
    [ testCase "unknown command instructs users how to get help" unknownCommandIncludesHelpHint
    , testCase "successful parse does not raise failure" successfulParseIsIntact
    ]

unknownCommandIncludesHelpHint :: Assertion
unknownCommandIncludesHelpHint =
  case parseCli ["frobnicate"] of
    Failure failure -> do
      let (message, exitCode) = OA.renderFailure failure "duet-rpc"
      exitCode @?= ExitFailure 1
      assertBool
        "error output should direct users to the subcommand help"
        (helpHint `T.isInfixOf` T.pack message)
    _ -> assertFailure "Expected parse failure for unknown command"
  where
    helpHint :: T.Text
    helpHint = "See 'duet-rpc <command> --help' for more information."

successfulParseIsIntact :: Assertion
successfulParseIsIntact =
  case parseCli ["version"] of
    Success CliOptions {} -> pure ()
    _ -> assertFailure "Expected parse success for known command"
