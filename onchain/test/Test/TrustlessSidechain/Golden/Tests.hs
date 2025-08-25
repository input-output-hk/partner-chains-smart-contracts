module Test.TrustlessSidechain.Golden.Tests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.Golden.Types qualified as Types
import Test.TrustlessSidechain.Golden.Versioning qualified as Versioning

tests :: TestTree
tests =
  testGroup
    "Golden tests of ToData encoded data"
    [ Types.tests
    , Versioning.tests
    ]
