module Main (main) where

import Test.Tasty
import Test.TrustlessSidechain.MerkleProofSerialisation as MerkleProofSerialisation
import Test.TrustlessSidechain.MerkleTree as MerkleTree
import Test.TrustlessSidechain.MultiSig as MultiSig
import Test.TrustlessSidechain.UpdateCommitteeHashMessage as UpdateCommitteeHashMessage
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
    [ MerkleTree.test
    , MultiSig.test
    , MerkleProofSerialisation.testSide
    , MerkleProofSerialisation.testRootHash
    , MerkleProofSerialisation.testUp
    , MerkleProofSerialisation.testCombinedMerkleProof
    , UpdateCommitteeHashMessage.test
    ]
