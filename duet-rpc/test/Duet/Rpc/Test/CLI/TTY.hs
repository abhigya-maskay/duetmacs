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
tests = testGroup "tty" [ttyHelpMatchesPlainOutput]

ttyHelpMatchesPlainOutput :: TestTree
ttyHelpMatchesPlainOutput =
  testCase "TTY help matches plain output" ttyHelpAssertion

ttyHelpAssertion :: Assertion
ttyHelpAssertion = do
  plain <- runCli defaultInvocation {cliArgs = ["--help"]}
  ttyResult <- runCliViaScript defaultInvocation {cliArgs = ["--help"]}
  case ttyResult of
    Left reason ->
      assertBool (T.unpack ("Skipping TTY test: " <> reason)) True
    Right result -> do
      containsAnsi (cliStdout result) @?= False
      containsAnsi (cliStdout plain) @?= False
      normalize (cliStdout plain) @?= normalize (cliStdout result)
      cliStderr plain @?= ""
      cliStderr result @?= ""
  where
    normalize :: T.Text -> T.Text
    normalize = T.replace "\r\n" "\n"
