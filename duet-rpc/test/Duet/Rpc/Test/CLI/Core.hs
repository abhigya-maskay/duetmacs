module Duet.Rpc.Test.CLI.Core
  ( tests
  ) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative (ParserResult (..))
import qualified Options.Applicative as OA
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , assertFailure
  , testCase
  , (@?=)
  )

import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliOptions (..)
  , CommandAction (..)
  , commandActionOf
  , commandDescriptions
  )
import Duet.Rpc.Test.CLI.Helpers (parseCli)

headerLine :: Text
headerLine = "duet-rpc [COMMAND] [OPTIONS]"

footerLine :: Text
footerLine = "See 'duet-rpc <command> --help' for more information."

expectedDescriptions :: [(CliCommand, String)]
expectedDescriptions =
  [ (CmdVersion, "Show version information")
  , (CmdDoctor, "Run diagnostics")
  , (CmdRpc, "Start RPC server")
  , (CmdPrompt, "Run prompt workflow")
  ]

expectedActions :: [(CliCommand, CommandAction)]
expectedActions =
  [ (CmdVersion, CommandActionVersion)
  , (CmdDoctor, CommandActionDoctor)
  , (CmdRpc, CommandActionRpc)
  , (CmdPrompt, CommandActionPrompt)
  ]

expectedInvocations :: [(String, CliCommand)]
expectedInvocations =
  [ ("version", CmdVersion)
  , ("doctor", CmdDoctor)
  , ("rpc", CmdRpc)
  , ("prompt", CmdPrompt)
  ]

tests :: TestTree
tests =
  testGroup
    "Core"
    [ testCase "commandInfos exposes all commands" commandRegistryCoverage
    , testCase "commandActionOf resolves registry" commandActionsResolve
    , testCase "parser recognizes subcommands" parserRecognizesSubcommands
    , testCase "parser renders documented header/footer" parserHeaderFooterStable
    ]

commandRegistryCoverage :: Assertion
commandRegistryCoverage =
  commandDescriptions @?= expectedDescriptions

commandActionsResolve :: Assertion
commandActionsResolve = traverse_ assertAction expectedActions
  where
    assertAction :: (CliCommand, CommandAction) -> Assertion
    assertAction (cmd, action) = commandActionOf cmd @?= Just action

parserRecognizesSubcommands :: Assertion
parserRecognizesSubcommands = traverse_ assertParse expectedInvocations
  where
    assertParse :: (String, CliCommand) -> Assertion
    assertParse (argument, expectedCommand) =
      case parseCli [argument] of
        Success options -> optCommand options @?= Just expectedCommand
        other ->
          assertFailure
            ( "Expected successful parse for "
                <> argument
                <> ", but got: "
                <> show other
            )

parserHeaderFooterStable :: Assertion
parserHeaderFooterStable =
  case parseCli ["--help"] of
    Failure failure -> do
      let (message, _) = OA.renderFailure failure "duet-rpc"
          nonEmptyLines = filter (not . T.null) (T.lines (T.pack message))
      assertBool "help output should contain header" (headerLine `elem` nonEmptyLines)
      assertBool "help output should contain footer" (footerLine `elem` nonEmptyLines)
    other -> assertFailure ("Expected parser failure rendering help, but got: " <> show other)
