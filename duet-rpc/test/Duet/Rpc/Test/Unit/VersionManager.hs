module Duet.Rpc.Test.Unit.VersionManager
  ( tests
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_duet_rpc (version)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Duet.Rpc.VersionManager (renderVersion)

tests :: TestTree
tests =
  testGroup
    "VersionManager"
    [ testCase "renderVersion mirrors Cabal package version" mirrorsPackageVersion
    , testCase "renderVersion never returns empty or newline-terminated text" givesAtomicVersionToken
    ]

mirrorsPackageVersion :: Assertion
mirrorsPackageVersion = renderVersion @?= expectedVersion
  where
    expectedVersion :: Text
    expectedVersion = T.pack (showVersion version)

givesAtomicVersionToken :: Assertion
givesAtomicVersionToken = do
  assertBool "renderVersion should not produce empty text" (not (T.null rendered))
  assertBool "renderVersion should not include a trailing newline" (not (T.isSuffixOf "\n" rendered))
  where
    rendered :: Text
    rendered = renderVersion
