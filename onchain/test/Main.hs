module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.TrustlessSidechain.Golden.Tests qualified as Golden
import Test.TrustlessSidechain.MerkleProofSerialisation qualified as MerkleProofSerialisation
import Test.TrustlessSidechain.MerkleTree qualified as MerkleTree
import Test.TrustlessSidechain.MultiSig qualified as MultiSig
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
    [ MerkleTree.test
    , MultiSig.test
    , Golden.tests
    , MerkleProofSerialisation.testSide
    , MerkleProofSerialisation.testRootHash
    , MerkleProofSerialisation.testUp
    , MerkleProofSerialisation.testCombinedMerkleProof
    ]
