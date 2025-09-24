module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Duet.Rpc.Test.CLI as CLI

main :: IO ()
main =
  defaultMain $ testGroup "duet-rpc" [CLI.tests]
