module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.TrustlessSidechain.MerkleProofSerialisation qualified as MerkleProofSerialisation
import Test.TrustlessSidechain.MerkleTree qualified as MerkleTree
import Test.TrustlessSidechain.MultiSig qualified as MultiSig
import Test.TrustlessSidechain.UpdateCommitteeHashMessage qualified as UpdateCommitteeHashMessage
import Prelude qualified as Haskell

-- | @since 0.1
main :: Haskell.IO ()
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
    , MerkleProofSerialisation.testSide
    , MerkleProofSerialisation.testRootHash
    , MerkleProofSerialisation.testUp
    , MerkleProofSerialisation.testCombinedMerkleProof
    , UpdateCommitteeHashMessage.test
    ]
