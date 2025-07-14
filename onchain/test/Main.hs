module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.TrustlessSidechain.Golden.Tests qualified as Golden
import Test.TrustlessSidechain.Transactions qualified as Transactions
import TrustlessSidechain.HaskellPrelude

main :: IO ()
main = defaultMain tests

-- | Project wide tests
tests :: TestTree
tests =
  testGroup
    "TrustlessSidechain"
    [ Golden.tests
    , Transactions.tests
    ]
