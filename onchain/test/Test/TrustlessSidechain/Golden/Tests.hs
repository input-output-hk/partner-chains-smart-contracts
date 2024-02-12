module Test.TrustlessSidechain.Golden.Tests (tests) where

import Data.String
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.Golden.DistributedSet qualified as DistributedSet
import Test.TrustlessSidechain.Golden.FUELProxyPolicy qualified as FUELProxyPolicy
import Test.TrustlessSidechain.Golden.Types qualified as Types
import Test.TrustlessSidechain.Golden.Versioning qualified as Versioning

tests :: TestTree
tests =
  testGroup
    "Golden tests of ToData encoded data"
    [ Types.tests
    , DistributedSet.tests
    , FUELProxyPolicy.tests
    , Versioning.tests
    ]
