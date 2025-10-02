module Duet.Rpc.Test.CLI.NoColor
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..),
    CliResult (..),
    containsAnsi,
    defaultInvocation,
    runCli,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "color"
    [ noColorEnvDisablesAnsi,
      noColorFlagDisablesAnsi
    ]

noColorEnvDisablesAnsi :: TestTree
noColorEnvDisablesAnsi =
  testCase "NO_COLOR env removes ANSI" $ do
    result <-
      runCli
        defaultInvocation
          { cliArgs = ["--help"],
            cliEnv = Map.singleton "NO_COLOR" "1"
          }
    containsAnsi (cliStdout result) @?= False

noColorFlagDisablesAnsi :: TestTree
noColorFlagDisablesAnsi =
  testCase "--no-color flag removes ANSI" $ do
    result <-
      runCli
        defaultInvocation
          { cliArgs = ["--no-color", "--help"]
          }
    containsAnsi (cliStdout result) @?= False
