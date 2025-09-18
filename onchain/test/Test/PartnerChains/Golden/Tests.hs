module Test.PartnerChains.Golden.Tests (tests) where

import Test.PartnerChains.Golden.Types qualified as Types
import Test.PartnerChains.Golden.Versioning qualified as Versioning
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Golden tests of ToData encoded data"
    [ Types.tests
    , Versioning.tests
    ]
