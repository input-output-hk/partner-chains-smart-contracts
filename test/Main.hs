module Main (main) where

import Test.Tasty

import Test.TrustlessSidechain.DistributedSet as DistributedSet
import Test.TrustlessSidechain.Integration as Integration
import Test.TrustlessSidechain.MerkleTree as MerkleTree
import Prelude (IO)

-- | @since 0.1
main :: IO ()
main = defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "TrustlessSidechain"
    [ Integration.test
    , MerkleTree.test
    , DistributedSet.test
    ]
