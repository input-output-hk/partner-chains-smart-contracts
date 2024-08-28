module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.TrustlessSidechain.Golden.Tests qualified as Golden
import TrustlessSidechain.HaskellPrelude

-- | @since 0.1
main :: IO ()
main = defaultMain tests

-- | Project wide tests
--
-- @since 0.1
tests :: TestTree
tests =
  testGroup
    "TrustlessSidechain"
    [ Golden.tests
    ]
