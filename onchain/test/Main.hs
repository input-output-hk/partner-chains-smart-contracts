module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.TrustlessSidechain.MerkleProofSerialisation qualified as MerkleProofSerialisation
import Test.TrustlessSidechain.MerkleTree qualified as MerkleTree
import Test.TrustlessSidechain.MultiSig qualified as MultiSig
import Test.TrustlessSidechain.Types qualified as Types
import TrustlessSidechain.HaskellPrelude

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
    [ MerkleTree.test
    , MultiSig.test
    , Types.tests
    , MerkleProofSerialisation.testSide
    , MerkleProofSerialisation.testRootHash
    , MerkleProofSerialisation.testUp
    , MerkleProofSerialisation.testCombinedMerkleProof
    ]
