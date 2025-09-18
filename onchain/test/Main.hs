module Main (main) where

import Test.PartnerChains.Golden.Tests qualified as Golden
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

-- | Project wide tests
tests :: TestTree
tests =
  testGroup
    "PartnerChains"
    [ Golden.tests
    ]
