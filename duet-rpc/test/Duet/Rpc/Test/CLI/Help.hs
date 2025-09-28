module Duet.Rpc.Test.CLI.Help
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , defaultInvocation
  , runCli
  )
import Duet.Rpc.Test.CLI.Assertions (assertTextContains)

tests :: TestTree
tests =
  testGroup
    "help"
    [ testCase "--help is available" helpFlagShowsSynopsis
    , testCase "no args displays help" noArgsShowsHelp
    ]

helpFlagShowsSynopsis :: Assertion
helpFlagShowsSynopsis = do
  result <- runCli defaultInvocation {cliArgs = ["--help"]}
  assertTextContains (cliStdout result) "duet-rpc [COMMAND] [OPTIONS]"
  cliStderr result @?= ""

noArgsShowsHelp :: Assertion
noArgsShowsHelp = do
  helpResult <- runCli defaultInvocation {cliArgs = ["--help"]}
  emptyResult <- runCli defaultInvocation
  cliStdout emptyResult @?= cliStdout helpResult
  cliStderr emptyResult @?= ""
