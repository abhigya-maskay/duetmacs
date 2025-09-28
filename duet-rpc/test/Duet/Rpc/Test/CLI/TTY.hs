module Duet.Rpc.Test.CLI.TTY
  ( tests
  ) where

import Data.Char (isLetter)
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
tests = testGroup "tty" [ttyDoctorMatchesPlainOutput]

ttyDoctorMatchesPlainOutput :: TestTree
ttyDoctorMatchesPlainOutput =
  testCase "TTY doctor matches plain output" ttyDoctorAssertion

ttyDoctorAssertion :: Assertion
ttyDoctorAssertion = do
  plain <- runCli defaultInvocation {cliArgs = ["doctor"]}
  ttyResult <- runCliViaScript defaultInvocation {cliArgs = ["doctor"]}
  case ttyResult of
    Left reason ->
      assertBool (T.unpack ("Skipping TTY test: " <> reason)) True
    Right result -> do
      containsAnsi (cliStdout result) @?= True
      containsAnsi (cliStdout plain) @?= False
      normalize (cliStdout plain) @?= normalize (stripAnsi (cliStdout result))
      cliStderr plain @?= ""
      cliStderr result @?= ""
  where
    normalize :: T.Text -> T.Text
    normalize = T.replace "\r\n" "\n"

    stripAnsi :: T.Text -> T.Text
    stripAnsi text =
      case T.breakOn "\ESC[" text of
        (before, remainder)
          | T.null remainder -> before
          | otherwise ->
              let rest = T.drop 2 remainder
                  (_, after) = T.span isAnsiChar rest
                  dropCount = if T.null after then 0 else 1
               in before <> stripAnsi (T.drop dropCount after)

    isAnsiChar :: Char -> Bool
    isAnsiChar ch = not (isLetter ch) && ch /= '\ESC'
