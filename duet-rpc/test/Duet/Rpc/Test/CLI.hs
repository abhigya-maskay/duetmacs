module Duet.Rpc.Test.CLI
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)

import qualified Duet.Rpc.Test.CLI.Golden as Golden
import qualified Duet.Rpc.Test.CLI.Help as Help
import qualified Duet.Rpc.Test.CLI.NoColor as NoColor
import qualified Duet.Rpc.Test.CLI.Errors as Errors
import qualified Duet.Rpc.Test.CLI.Version as Version
import qualified Duet.Rpc.Test.CLI.TTY as TTY

tests :: TestTree
tests =
  testGroup
    "CLI"
    [ Version.tests
    , Help.tests
    , Errors.tests
    , Golden.tests
    , NoColor.tests
    , TTY.tests
    ]
