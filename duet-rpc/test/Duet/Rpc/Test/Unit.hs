module Duet.Rpc.Test.Unit
  ( tests
  ) where

import Test.Tasty (TestTree, testGroup)

import qualified Duet.Rpc.Test.Unit.ErrorHandler as ErrorHandler
import qualified Duet.Rpc.Test.Unit.OutputFormatter as OutputFormatter
import qualified Duet.Rpc.Test.Unit.VersionManager as VersionManager

tests :: TestTree
tests =
  testGroup
    "unit"
    [ VersionManager.tests
    , OutputFormatter.tests
    , ErrorHandler.tests
    ]
