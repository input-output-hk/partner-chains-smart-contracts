module Test.Checkpoint
  ( saveCheckpointTest
  -- , testScenario2
  -- , testScenario3
  -- , testScenario4
  -- , updateCommitteeHash
  -- , updateCommitteeHashWith
  , tests
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (liftContractM)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.InitSidechain (InitSidechainParams(..), initSidechain)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Crypto
  ( SidechainPrivateKey
  , SidechainPublicKey
  , SidechainSignature
  , generatePrivKey
  , multiSign
  , toPubKeyUnsafe
  )

generateCheckpointSignatures ∷
  { sidechainParams ∷ SidechainParams
  , currentCommitteePrvKeys ∷ Array SidechainPrivateKey
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  } →
  Maybe (Array (Tuple SidechainPublicKey SidechainSignature))
generateCheckpointSignatures
  { sidechainParams
  , currentCommitteePrvKeys
  , newCheckpointBlockHash
  , newCheckpointBlockNumber
  , sidechainEpoch
  } = do
  let
    -- Order the private keys by lexicographical ordering of the signatures, so
    -- it's easy to give the sorted pubkey with its associated signature.
    currentCommitteePubKeys /\ currentCommitteePrvKeys' =
      Array.unzip
        $ Array.sortWith fst
        $ map (\prvKey → toPubKeyUnsafe prvKey /\ prvKey) currentCommitteePrvKeys

  checkpointMessage ← Checkpoint.serialiseCheckpointMessage $
    Checkpoint.CheckpointMessage
      { sidechainParams: sidechainParams
      , checkpointBlockHash: newCheckpointBlockHash
      , checkpointBlockNumber: newCheckpointBlockNumber
      , sidechainEpoch: sidechainEpoch
      }

  let
    committeeSignatures = Array.zip
      currentCommitteePubKeys
      (multiSign currentCommitteePrvKeys' checkpointMessage)

  pure committeeSignatures

tests ∷ WrappedTests
tests = plutipGroup "Checkpointing" $ do
  saveCheckpointTest
  invalidCommitteeTest
  notEnoughSignaturesTest

saveCheckpointTest ∷ PlutipTest
saveCheckpointTest = Mote.Monad.test "Save checkpoint (happy path)"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      logInfo' "Checkpoint 'saveCheckpointTest'"
      genesisUtxo ← Test.Utils.getOwnTransactionInput
      let
        keyCount = 25
      initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
      let
        initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
        initScParams = InitSidechainParams
          { initChainId: BigInt.fromInt 1
          , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
          , initUtxo: genesisUtxo
          , initCommittee: initCommitteePubKeys
          , initSidechainEpoch: zero
          , initThresholdNumerator: BigInt.fromInt 2
          , initThresholdDenominator: BigInt.fromInt 3
          }

      { sidechainParams } ← initSidechain initScParams

      let
        newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
        newCheckpointBlockNumber = BigInt.fromInt 1
        sidechainEpoch = BigInt.fromInt 0 -- same epoch checkpoint
        toSign =
          { sidechainParams
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , newCheckpointBlockHash
          , newCheckpointBlockNumber
          , sidechainEpoch
          }

      committeeSignatures ←
        liftContractM
          "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
          $ generateCheckpointSignatures toSign

      let
        saveCheckpointInput = Checkpoint.CheckpointEndpointParam
          { sidechainParams
          , committeeSignatures: map (Just <$> _) committeeSignatures
          , newCheckpointBlockHash
          , newCheckpointBlockNumber
          , sidechainEpoch
          }

      void $ Checkpoint.saveCheckpoint saveCheckpointInput

invalidCommitteeTest ∷ PlutipTest
invalidCommitteeTest =
  Mote.Monad.test "Save checkpoint should fail if the committee is invalid"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        logInfo' "Checkpoint 'invalidCommitteeTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        let
          keyCount = 10
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initScParams = InitSidechainParams
            { initChainId: BigInt.fromInt 1
            , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initCommittee: initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            }

        { sidechainParams } ← initSidechain initScParams

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 0 -- same epoch checkpoint
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          liftContractM
            "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            $ generateCheckpointSignatures toSign

        --- Remove three committee members from the committee
        let
          committeeSignatures' = Array.take 2 committeeSignatures

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , committeeSignatures: map (Just <$> _) committeeSignatures'
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        Test.Utils.fails $ void $ Checkpoint.saveCheckpoint saveCheckpointInput

notEnoughSignaturesTest ∷ PlutipTest
notEnoughSignaturesTest =
  Mote.Monad.test
    "Save checkpoint should fail if there are not enough signatures"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        logInfo' "Checkpoint 'notEnoughSignaturesTest'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        let
          keyCount = 5
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initScParams = InitSidechainParams
            { initChainId: BigInt.fromInt 1
            , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initCommittee: initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            }

        { sidechainParams } ← initSidechain initScParams

        let
          newCheckpointBlockHash = hexToByteArrayUnsafe "aabbccdd"
          newCheckpointBlockNumber = BigInt.fromInt 1
          sidechainEpoch = BigInt.fromInt 0 -- same epoch checkpoint
          toSign =
            { sidechainParams
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        committeeSignatures ←
          liftContractM
            "error 'Test.Checkpoint.saveCheckpoint': failed to generate the committee signatures for the checkpoint message"
            $ generateCheckpointSignatures toSign

        let
          committeeSignatures' = map (Just <$> _) committeeSignatures
          notEnoughSignatures = mapWithIndex
            ( \idx (Tuple pk sig) →
                if idx < 3 then Tuple pk Nothing
                else Tuple pk sig
            )
            committeeSignatures'

        let
          saveCheckpointInput = Checkpoint.CheckpointEndpointParam
            { sidechainParams
            , committeeSignatures: notEnoughSignatures
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }

        Test.Utils.fails $ void $ Checkpoint.saveCheckpoint saveCheckpointInput

-- | `testScenario2` updates the committee hash with a threshold ratio of 1/1,
-- | but should fail because there isn't enough committee members signing the update
-- | off.
-- testScenario2 ∷ PlutipTest
-- testScenario2 =
--   Mote.Monad.test "Update committee hash without honest majority (should fail)"
--     $ Test.PlutipTest.mkPlutipConfigTest
--         [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
--     $ \alice → Wallet.withKeyWallet alice do
--         logInfo' "UpdateCommitteeHash 'testScenario2'"
--         genesisUtxo ← Test.Utils.getOwnTransactionInput
--         let
--           keyCount = 2
--         -- woohoo!! smaller committee size so it's easy to remove the majority
--         -- sign below, and make this test case fail...
--         initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
--         let
--           initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
--           initScParams = InitSidechainParams
--             { initChainId: BigInt.fromInt 1
--             , initGenesisHash: hexToByteArrayUnsafe
--                 "aabbccddeeffgghhiijjkkllmmnnoo"
--             , initUtxo: genesisUtxo
--             , initCommittee: initCommitteePubKeys
--             , initThresholdNumerator: BigInt.fromInt 1
--             , initThresholdDenominator: BigInt.fromInt 1
--             , initSidechainEpoch: BigInt.fromInt 0
--             }
--
--         { sidechainParams: scParams } ← initSidechain initScParams
--         nextCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
--
--         Test.Utils.fails
--           $ updateCommitteeHashWith
--               { sidechainParams: scParams
--               , currentCommitteePrvKeys: initCommitteePrvKeys
--               , newCommitteePrvKeys: nextCommitteePrvKeys
--               , previousMerkleRoot: Nothing
--               , sidechainEpoch: BigInt.fromInt 1
--               }
--           $ \(UpdateCommitteeHashParams params) →
--               pure
--                 $ UpdateCommitteeHashParams
--                 $ params
--                     { committeeSignatures =
--                         Unsafe.unsafePartial
--                           ( case params.committeeSignatures of
--                               [ c1 /\ _s1
--                               , c2 /\ s2
--                               ] →
--                                 [ c1 /\ Nothing
--                                 , c2 /\ s2
--                                 ]
--                           )
--                     }

-- | `testScenario3` initialises the committee with an out of order committee
-- | (by moving the smallest committee member to the end), and updates the committee
-- | hash when the signatures / new committee are given out of order (in
-- | reverse order actually); and updates it again
-- testScenario3 ∷ PlutipTest
-- testScenario3 =
--   Mote.Monad.test "Update committee hash with out of order committee"
--     $ Test.PlutipTest.mkPlutipConfigTest
--         [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
--     $ \alice → Wallet.withKeyWallet alice do
--         logInfo' "UpdateCommitteeHash 'testScenario3'"
--         genesisUtxo ← Test.Utils.getOwnTransactionInput
--         let
--           keyCount = 25
--         initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
--         let
--           initCommitteePubKeys = Array.sort
--             (map toPubKeyUnsafe initCommitteePrvKeys)
--           initScParams = InitSidechainParams
--             { initChainId: BigInt.fromInt 6
--             , initGenesisHash: hexToByteArrayUnsafe "aabbccdd"
--             , initUtxo: genesisUtxo
--             , initCommittee: case Array.uncons initCommitteePubKeys of
--                 Nothing → mempty
--                 Just { head, tail } → tail <> Array.singleton head
--             -- note how we mess up the order of the initial public keys
--             -- assuming that all entries are distinct (which should be the case
--             -- with high probability)
--             , initSidechainEpoch: zero
--             , initThresholdNumerator: BigInt.fromInt 2
--             , initThresholdDenominator: BigInt.fromInt 3
--             }
--
--         { sidechainParams } ← initSidechain initScParams
--         nextCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
--         nextNextCommitteePrvKeys ← sequence $ Array.replicate keyCount
--           generatePrivKey
--
--         let
--           reverseSignaturesAndNewCommittee ∷
--             UpdateCommitteeHashParams → UpdateCommitteeHashParams
--           reverseSignaturesAndNewCommittee uchp =
--             wrap
--               ( (unwrap uchp)
--                   { committeeSignatures = Array.reverse
--                       ((unwrap uchp).committeeSignatures)
--                   , newCommitteePubKeys = Array.reverse
--                       ((unwrap uchp).newCommitteePubKeys)
--                   }
--               )
--
--         -- the first update
--         updateCommitteeHashWith
--           { sidechainParams
--           , currentCommitteePrvKeys: initCommitteePrvKeys
--           , newCommitteePrvKeys: nextCommitteePrvKeys
--           , previousMerkleRoot: Nothing
--           , sidechainEpoch: BigInt.fromInt 1
--           }
--           (pure <<< reverseSignaturesAndNewCommittee)
--
--         -- the second update
--         updateCommitteeHash
--           { sidechainParams
--           , currentCommitteePrvKeys: nextCommitteePrvKeys
--           , newCommitteePrvKeys: nextNextCommitteePrvKeys
--           , previousMerkleRoot: Nothing
--           , sidechainEpoch: BigInt.fromInt 2
--           }

-- | `testScenario4` is given in #277
-- testScenario4 ∷ PlutipTest
-- testScenario4 =
--   Mote.Monad.test "Unsorted committee members (issue #277)"
--     $ Test.PlutipTest.mkPlutipConfigTest
--         [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
--     $ \alice → Wallet.withKeyWallet alice do
--         logInfo' "UpdateCommitteeHash 'testScenario3'"
--         genesisUtxo ← Test.Utils.getOwnTransactionInput
--
--         -- the committees as given in the test case
--         let
--           initCommitteePrvKeys =
--             [ byteArrayToSidechainPrivateKeyUnsafe $ hexToByteArrayUnsafe
--                 "3e77009e691a2c38c53d5c0608af90af5c793efaa6cfe9e8670b141ed0376911"
--             , byteArrayToSidechainPrivateKeyUnsafe $ hexToByteArrayUnsafe
--                 "d9465fedde9190b2760bb37ac2b89cf97d7121a98807f8849532e58750d23725"
--             , byteArrayToSidechainPrivateKeyUnsafe $ hexToByteArrayUnsafe
--                 "3563a2e4d2b373b4b8ea0397b7437e7386d3d39216a77fa3ceb8f64a43d98f56"
--             ]
--           nextCommitteePrvKeys =
--             [ byteArrayToSidechainPrivateKeyUnsafe $ hexToByteArrayUnsafe
--                 "1b7267b5d84a108d67bd8cdc95750d135c1a1fb6482531ddfa0923c043b308f1"
--             , byteArrayToSidechainPrivateKeyUnsafe $ hexToByteArrayUnsafe
--                 "173d5d8cd43bd6119c633e654d00bebc2165e6875190b132dc93d5ee1b7d2448"
--             , byteArrayToSidechainPrivateKeyUnsafe $ hexToByteArrayUnsafe
--                 "34edb67b9f73389280214dae93e62074a9fcfd1eefadd4406cd7d27fd64b46a8"
--             ]
--
--           -- initialising the committee (translated from the CLI command
--           initScParams = InitSidechainParams
--             { initChainId: BigInt.fromInt 78
--             , initGenesisHash: hexToByteArrayUnsafe
--                 "d8063cc6e907f497360ca50238af5c2e2a95a8869a2ce74ab3e75fe6c9dcabd0"
--             , initUtxo: genesisUtxo
--             , initCommittee:
--                 [ byteArrayToSidechainPublicKeyUnsafe $ hexToByteArrayUnsafe
--                     "03d9e83bde65acf38fc97497210d7e6f6a1aebf5d4cd9b193c90b81a81c55bc678"
--                 , byteArrayToSidechainPublicKeyUnsafe $ hexToByteArrayUnsafe
--                     "03885cccf474b81faba56097f58fcca98a3c8986bc09cdbd163e54add33561f34c"
--                 , byteArrayToSidechainPublicKeyUnsafe $ hexToByteArrayUnsafe
--                     "032aa087b8e4a983a7220e1d2eb2db6a6bf8fbed9fad7f5af6824e05f0017c69e0"
--                 ]
--             , initSidechainEpoch: one
--             , initThresholdNumerator: BigInt.fromInt 2
--             , initThresholdDenominator: BigInt.fromInt 3
--             }
--
--         { sidechainParams } ← initSidechain initScParams
--
--         updateCommitteeHashWith
--           { sidechainParams
--           , currentCommitteePrvKeys: initCommitteePrvKeys
--           , newCommitteePrvKeys: nextCommitteePrvKeys
--           , previousMerkleRoot: Nothing
--           , sidechainEpoch: BigInt.fromInt 2
--           }
--           \uchp →
--             pure $ wrap $ (unwrap uchp)
--               { newCommitteePubKeys =
--                   [ byteArrayToSidechainPublicKeyUnsafe $ hexToByteArrayUnsafe
--                       "02b37ba1e0a18e8b3723e57fb6b220836ba6417ab75296f08f717106ad731ac47b"
--                   , byteArrayToSidechainPublicKeyUnsafe $ hexToByteArrayUnsafe
--                       "02cb793bcfcab7f17453f4c5e0e07a2818c6df4d7995aa1b7a0f0b219c6cfe0e20"
--                   , byteArrayToSidechainPublicKeyUnsafe $ hexToByteArrayUnsafe
--                       "0377c83c74fbccf05671697bf343a71a9c221568721c8e77f330fe82e9b08fdfea"
--                   ]
--               -- the signatures from the issue arne't quite right (since it
--               -- didn't order the committee), so we won't include those
--               -- signatures
--               }
