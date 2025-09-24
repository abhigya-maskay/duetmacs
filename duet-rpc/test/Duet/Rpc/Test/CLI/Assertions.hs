module Duet.Rpc.Test.CLI.Assertions
  ( assertTextContains
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty.HUnit (Assertion, (@?=))

assertTextContains :: Text -> Text -> Assertion
assertTextContains haystack needle = T.isInfixOf needle haystack @?= True
