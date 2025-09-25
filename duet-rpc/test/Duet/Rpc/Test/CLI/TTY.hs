module Duet.Rpc.Test.CLI.TTY
  ( tests
  ) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertBool
  , testCase
  , (@?=)
  )

import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , containsAnsi
  , defaultInvocation
  , runCli
  , runCliViaScript
  )

tests :: TestTree
tests = testGroup "tty" [ttyHelpShowsColor]

ttyHelpShowsColor :: TestTree
ttyHelpShowsColor =
  testCase "TTY captures ANSI while pipe stays plain" ttyHelpAssertion

ttyHelpAssertion :: Assertion
ttyHelpAssertion = do
  plain <- runCli defaultInvocation {cliArgs = ["--help"]}
  ttyResult <- runCliViaScript defaultInvocation {cliArgs = ["--help"]}
  case ttyResult of
    Left reason ->
      assertBool (T.unpack ("Skipping TTY test: " <> reason)) True
    Right result -> do
      containsAnsi (cliStdout result) @?= True
      containsAnsi (cliStdout plain) @?= False
      assertBool "TTY output should differ due to ANSI sequences" (cliStdout plain /= cliStdout result)
      cliStderr plain @?= ""
      cliStderr result @?= ""
