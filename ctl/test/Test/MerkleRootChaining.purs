-- | `Test.MerkleRootChaining` includes tests which demonstrate the merkle root
-- | chaining with both updating the committee hash and creating new merkle roots.
module Test.MerkleRootChaining where

import Contract.Prelude

import Contract.Address as Address
import Contract.Log as Log
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray as ByteArray
import Data.Array as Array
import Data.BigInt as BigInt
import FUELMintingPolicy (MerkleTreeEntry(MerkleTreeEntry))
import InitSidechain as InitSidechain
import SidechainParams (InitSidechainParams(InitSidechainParams))
import Test.MPTRoot as Test.MPTRoot
import Test.UpdateCommitteeHash as Test.UpdateCommitteeHash
import Test.Utils as Test.Utils
import UpdateCommitteeHash
  ( UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto

-- | `testScenario1` demonstrates (should succeed)
-- |    1. Initializing the sidechain with a committee.
-- |    2. Saving a merkle root
-- |    3. Update the committee hash
-- |    4. Update the committee hash
-- |    5. Saving a merkle root
-- |    6. Saving another merkle root
-- |    7. Updating the committee hash again
-- | Note how this demonstrates "working" behavior for arbitrarily many (well,
-- | 0-2) "saving merkle root" actions between the update committee hash
-- | actions
testScenario1 ∷ Contract () Unit
testScenario1 = do
  Log.logInfo' "Testing 'Test.MerkleRootChaining.testScenario1'"
  ownPaymentPubKeyHash ← Monad.liftedM
    "error 'Test.MerkleRootChaining.testScenario1': 'Contract.Address.ownPaymentPubKeyHash' failed"
    Address.ownPaymentPubKeyHash

  -- 1. Initializing the sidechain
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario1': 1. Initializing the sidechain"
  genesisUtxo ← Test.Utils.getOwnTransactionInput

  let keyCount = 25
  committee1PrvKeys ← sequence $ Array.replicate keyCount
    Utils.Crypto.generatePrivKey

  { sidechainParams } ← InitSidechain.initSidechain $
    InitSidechainParams
      { initChainId: BigInt.fromInt 69_420
      , initGenesisHash: ByteArray.hexToByteArrayUnsafe "aabbcc"
      , initUtxo: genesisUtxo
      , initCommittee: map Utils.Crypto.toPubKeyUnsafe committee1PrvKeys
      , initSidechainEpoch: zero
      , initThresholdNumerator: BigInt.fromInt 2
      , initThresholdDenominator: BigInt.fromInt 3
      }

  -- 2. Saving a merkle root.
  -------------------------------
  Log.logInfo' "'Test.MerkleRootChaining.testScenario1': 2. saving a merkle root"
  { merkleRoot: merkleRoot2 } ← Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries:
        [ MerkleTreeEntry
            { index: BigInt.fromInt 0
            , amount: BigInt.fromInt 69
            , previousMerkleRoot: Nothing
            , recipient: Test.Utils.paymentPubKeyHashToByteArray
                ownPaymentPubKeyHash
            }
        ]
    , currentCommitteePrvKeys: committee1PrvKeys
    , previousMerkleRoot: Nothing
    }

  -- 3. Updating the committee hash
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario1': 3. updating the committee hash"
  committee3PrvKeys ← sequence $ Array.replicate keyCount
    Utils.Crypto.generatePrivKey
  Test.UpdateCommitteeHash.updateCommitteeHash
    { sidechainParams
    , currentCommitteePrvKeys: committee1PrvKeys
    , newCommitteePrvKeys: committee3PrvKeys
    , previousMerkleRoot: Just merkleRoot2
    , sidechainEpoch: BigInt.fromInt 1
    }

  -- 4. Updating the committee hash
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario1': 4. updating the committee hash"
  committee4PrvKeys ← sequence $ Array.replicate keyCount
    Utils.Crypto.generatePrivKey
  Test.UpdateCommitteeHash.updateCommitteeHash
    { sidechainParams
    , currentCommitteePrvKeys: committee3PrvKeys
    , newCommitteePrvKeys: committee4PrvKeys
    , -- Note: this is the same merkle root as the last committee update.
      previousMerkleRoot: Just merkleRoot2
    , sidechainEpoch: BigInt.fromInt 2
    }

  -- 5. Saving a merkle root
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario1': 5. saving the merkle root"
  { merkleRoot: merkleRoot5 } ← Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries:
        [ MerkleTreeEntry
            { index: BigInt.fromInt 0
            , amount: BigInt.fromInt 69
            , -- Note: this is the same merkle root as used in 4.
              previousMerkleRoot: Just merkleRoot2
            , recipient: Test.Utils.paymentPubKeyHashToByteArray
                ownPaymentPubKeyHash
            }
        ]
    , -- Note: the current committee is from 4.
      currentCommitteePrvKeys: committee4PrvKeys
    , previousMerkleRoot: Just merkleRoot2
    }

  -- 6. Saving a merkle root
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario1': 6. saving the merkle root"
  { merkleRoot: merkleRoot6 } ← Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries:
        [ MerkleTreeEntry
            { index: BigInt.fromInt 0
            , amount: BigInt.fromInt 69
            , -- Note: this is the same merkle root as used in 5.
              previousMerkleRoot: Just merkleRoot5
            , recipient: Test.Utils.paymentPubKeyHashToByteArray
                ownPaymentPubKeyHash
            }
        ]
    , -- Note: the current committee is from 4.
      currentCommitteePrvKeys:
        committee4PrvKeys
    , previousMerkleRoot: Just merkleRoot5
    }

  -- 7. Updating the committee hash
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario1': 7. updating the committee hash"
  committee7PrvKeys ← sequence $ Array.replicate keyCount
    Utils.Crypto.generatePrivKey
  Test.UpdateCommitteeHash.updateCommitteeHash
    { sidechainParams
    , currentCommitteePrvKeys: committee4PrvKeys
    , newCommitteePrvKeys: committee7PrvKeys
    , previousMerkleRoot: Just merkleRoot6
    , sidechainEpoch: BigInt.fromInt 3
    }

  pure unit

-- | `testScenario2` demonstrates (should fail)
-- |    1. Initializing the sidechain with a committee.
-- |    2. Saving a merkle root
-- |    3. Attempt (but fail) to update the committee hash with the merkle root
-- |    as `Nothing`
testScenario2 ∷ Contract () Unit
testScenario2 = do
  Log.logInfo' "Testing 'Test.MerkleRootChaining.testScenario2'"
  ownPaymentPubKeyHash ← Monad.liftedM
    "error 'Test.MerkleRootChaining.testScenario1': 'Contract.Address.ownPaymentPubKeyHash' failed"
    Address.ownPaymentPubKeyHash

  -- 1. Initializing the sidechain
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario2': 1. Initializing the sidechain"
  genesisUtxo ← Test.Utils.getOwnTransactionInput

  let keyCount = 25
  committee1PrvKeys ← sequence $ Array.replicate keyCount
    Utils.Crypto.generatePrivKey

  { sidechainParams } ← InitSidechain.initSidechain $
    InitSidechainParams
      { initChainId: BigInt.fromInt 69_420
      , initGenesisHash: ByteArray.hexToByteArrayUnsafe "aabbcc"
      , initUtxo: genesisUtxo
      , initCommittee: map Utils.Crypto.toPubKeyUnsafe committee1PrvKeys
      , initSidechainEpoch: zero
      , initThresholdNumerator: BigInt.fromInt 2
      , initThresholdDenominator: BigInt.fromInt 3
      }

  -- 2. Saving a merkle root
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario2': 2. saving the merkle root"
  { merkleRoot: merkleRoot2 } ← Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries:
        [ MerkleTreeEntry
            { index: BigInt.fromInt 0
            , amount: BigInt.fromInt 69
            , previousMerkleRoot: Nothing
            , recipient: Test.Utils.paymentPubKeyHashToByteArray
                ownPaymentPubKeyHash
            }
        ]
    , currentCommitteePrvKeys: committee1PrvKeys
    , previousMerkleRoot: Nothing
    }

  -- 3. Updating the committee hash with the wrong merkle root.
  -------------------------------
  Log.logInfo'
    "'Test.MerkleRootChaining.testScenario2': 3. updating the committee hash incorrectly"
  -- create a new committee
  committee3PrvKeys ← sequence $ Array.replicate keyCount
    Utils.Crypto.generatePrivKey
  let
    committee1PubKeys = map Utils.Crypto.toPubKeyUnsafe committee1PrvKeys
    committee3PubKeys = map Utils.Crypto.toPubKeyUnsafe committee3PrvKeys
  -- the message updates committee1 to be committee3
  committee1Message ←
    Monad.liftContractM
      "error 'Test.MerkleRootChaining.testScenario2': failed to serialise and hash update committee hash message"
      $ UpdateCommitteeHash.serialiseUchmHash
      $ UpdateCommitteeHashMessage
          { sidechainParams: sidechainParams
          , newCommitteePubKeys: committee3PubKeys
          ,
            -- Note: since we can trust the committee will sign the "correct" root,
            -- we necessarily know that the message that they sign should be
            -- the previousMerkleRoot which is `merkleRoot2` in this case.
            previousMerkleRoot: Just merkleRoot2
          , sidechainEpoch: BigInt.fromInt 1
          }

  Test.Utils.fails
    $ void
    $ UpdateCommitteeHash.updateCommitteeHash
    $
      UpdateCommitteeHashParams
        { sidechainParams
        , newCommitteePubKeys: committee3PubKeys
        , committeeSignatures: Array.zip
            committee1PubKeys
            (Just <$> Utils.Crypto.multiSign committee1PrvKeys committee1Message)
        ,
          -- Note: this is the EVIL thing -- we try to update the
          -- committee hash without really putting in the previous merkle
          -- root
          previousMerkleRoot: Nothing
        , sidechainEpoch: BigInt.fromInt 1
        }
