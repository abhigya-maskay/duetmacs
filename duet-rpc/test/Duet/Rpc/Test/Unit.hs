module Duet.Rpc.Test.Unit
  ( tests,
  )
where

import qualified Duet.Rpc.Test.Unit.ConfigLoader as ConfigLoader
import qualified Duet.Rpc.Test.Unit.ErrorHandler as ErrorHandler
import qualified Duet.Rpc.Test.Unit.OutputFormatter as OutputFormatter
import qualified Duet.Rpc.Test.Unit.VersionManager as VersionManager
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "unit"
    [ VersionManager.tests,
      OutputFormatter.tests,
      ErrorHandler.tests,
      ConfigLoader.tests
    ]
