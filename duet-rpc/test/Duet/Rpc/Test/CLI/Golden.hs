module Duet.Rpc.Test.CLI.Golden
  ( tests,
  )
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..),
    CliResult (..),
    defaultInvocation,
    runCli,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
  testGroup
    "golden"
    [goldenHelpPlain]

goldenHelpPlain :: TestTree
goldenHelpPlain =
  goldenVsString
    "plain help matches snapshot"
    "test/golden/help_plain.txt"
    renderHelp

renderHelp :: IO BL.ByteString
renderHelp = do
  result <- runCli invocation
  if cliStderr result /= ""
    then fail (T.unpack (cliStderr result))
    else pure (BL.fromStrict (TE.encodeUtf8 (cliStdout result)))
  where
    invocation =
      defaultInvocation
        { cliArgs = ["--help"],
          cliEnv = Map.singleton "NO_COLOR" "1"
        }
