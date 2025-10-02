module Main (main) where

import qualified Duet.Rpc.Test.CLI as CLI
import qualified Duet.Rpc.Test.Unit as Unit
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $ testGroup "duet-rpc" [CLI.tests, Unit.tests]
