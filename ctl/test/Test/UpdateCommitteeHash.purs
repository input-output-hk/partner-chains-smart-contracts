module Test.UpdateCommitteeHash
  ( testScenario1
  , testScenario2
  , updateCommitteeHash
  , updateCommitteeHashWith
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import InitSidechain (initSidechain)
import Partial.Unsafe as Unsafe
import SidechainParams (InitSidechainParams(..), SidechainParams)
import SidechainParams as SidechainParams
import Test.Utils as Test.Utils
import UpdateCommitteeHash
  ( UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(..)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto (PrivateKey, generatePrivKey, multiSign, toPubKeyUnsafe)
import Types (PubKey, Signature)

-- | `generateUchmSignatures` generates the public keys and corresponding
-- | signatures of the current committee for the new committee given.
generateUchmSignatures ::
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array PrivateKey
    , -- the last merkle root
    previousMerkleRoot ∷ Maybe ByteArray
    , -- the sidechain epoch
    sidechainEpoch ∷ BigInt
  } → Maybe (Array (Tuple PubKey Signature))
generateUchmSignatures
  { sidechainParams
  , currentCommitteePrvKeys
  , newCommitteePrvKeys
  , previousMerkleRoot
  , sidechainEpoch
  } = do
      let
        -- Order the private keys by lexicographical ordering of the signatures, so
        -- it's easy to give the sorted pubkey with its associated signature.
        currentCommitteePubKeys /\ currentCommitteePrvKeys' =
          Array.unzip
            $ Array.sortWith fst
            $ map (\prvKey → toPubKeyUnsafe prvKey /\ prvKey) currentCommitteePrvKeys

        newCommitteePubKeys = Array.sort $ map toPubKeyUnsafe newCommitteePrvKeys

      committeeMessage <-
          UpdateCommitteeHash.serialiseUchmHash
          $ UpdateCommitteeHashMessage
              { sidechainParams: SidechainParams.convertSCParams sidechainParams
              , newCommitteePubKeys
              , previousMerkleRoot
              , sidechainEpoch
              }
      let
        committeeSignatures = Array.zip
          currentCommitteePubKeys
          (multiSign currentCommitteePrvKeys' committeeMessage)

      pure committeeSignatures

-- | 'updateCommitteeHash' is a convenient wrapper around
-- 'UpdateCommitteeHash.updateCommitteeHash' for writing tests.
-- Note that this makes the entire current committee sign the message.
updateCommitteeHash ∷
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array PrivateKey
  , -- the last merkle root
    previousMerkleRoot ∷ Maybe ByteArray
  , -- sidechain epoch of the new committee
    sidechainEpoch ∷ BigInt
  } →
  Contract () Unit
updateCommitteeHash params = updateCommitteeHashWith params pure

-- | @'updateCommitteeHashWith' params f@ is a convenient wrapper around
-- 'UpdateCommitteeHash.updateCommitteeHash' for writing tests which modify the
-- inputted 'UpdateCommitteeHashParams' with the given function @f@.
--
-- In particular, the function @f@ can be used to change the signatures
-- provided by the committee.
updateCommitteeHashWith ∷
  { sidechainParams ∷ SidechainParams
  ,
    -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array PrivateKey
  , -- The new committee
    newCommitteePrvKeys ∷ Array PrivateKey
  , -- the last merkle root
    previousMerkleRoot ∷ Maybe ByteArray
  , -- sidechain epoch of the new committee
    sidechainEpoch ∷ BigInt
  } →
  (UpdateCommitteeHashParams → Contract () UpdateCommitteeHashParams) →
  Contract () Unit
updateCommitteeHashWith params f = void do
  committeeSignatures ←
    liftContractM
      "error 'Test.UpdateCommitteeHash.updateCommitteeHash': failed to generate the committee signatures for the committee hash message"
      $ generateUchmSignatures params

  let
    newCommitteePubKeys = Array.sort $ map toPubKeyUnsafe $ params.newCommitteePrvKeys
    uchp =
      UpdateCommitteeHashParams
        { sidechainParams: params.sidechainParams
        , newCommitteePubKeys
        , committeeSignatures:
            map (Just <$> _) committeeSignatures
            -- take `pubkey /\ sig` and convert to `pubkey /\ Just sig`
        , previousMerkleRoot: params.previousMerkleRoot
        , sidechainEpoch: params.sidechainEpoch
        }

  uchp' ← f uchp

  UpdateCommitteeHash.updateCommitteeHash uchp'

-- | 'testScenario1' updates the committee hash
testScenario1 ∷ Contract () Unit
testScenario1 = do
  logInfo' "UpdateCommitteeHash 'testScenario1'"
  genesisUtxo ← Test.Utils.getOwnTransactionInput
  let
    keyCount = 25
  initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let
    initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
    initScParams = InitSidechainParams
      { initChainId: BigInt.fromInt 1
      , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
      , initMint: Nothing
      , initUtxo: genesisUtxo
      , initCommittee: initCommitteePubKeys
      , initSidechainEpoch: zero
      , initThresholdNumerator: BigInt.fromInt 2
      , initThresholdDenominator: BigInt.fromInt 3
      }

  { sidechainParams } ← initSidechain initScParams
  nextCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey

  updateCommitteeHash
    { sidechainParams
    , currentCommitteePrvKeys: initCommitteePrvKeys
    , newCommitteePrvKeys: nextCommitteePrvKeys
    , previousMerkleRoot: Nothing
    , sidechainEpoch: BigInt.fromInt 1
    }

-- | 'testScenario2' updates the committee hash with a threshold ratio of 1/1,
-- but should fail because there isn't enough committee members signing the update
-- off.
testScenario2 ∷ Contract () Unit
testScenario2 = do
  logInfo' "UpdateCommitteeHash 'testScenario2'"
  genesisUtxo ← Test.Utils.getOwnTransactionInput
  let
    keyCount = 2
  -- woohoo!! smaller committee size so it's easy to remove the majority
  -- sign below, and make this test case fail...
  initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let
    initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
    initScParams = InitSidechainParams
      { initChainId: BigInt.fromInt 1
      , initGenesisHash: hexToByteArrayUnsafe "aabbccddeeffgghhiijjkkllmmnnoo"
      , initMint: Nothing
      , initUtxo: genesisUtxo
      , initCommittee: initCommitteePubKeys
      , initThresholdNumerator: BigInt.fromInt 1
      , initThresholdDenominator: BigInt.fromInt 1
      , initSidechainEpoch: BigInt.fromInt 0
      }

  { sidechainParams: scParams } ← initSidechain initScParams
  nextCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey

  Test.Utils.fails
    $ updateCommitteeHashWith
        { sidechainParams: scParams
        , currentCommitteePrvKeys: initCommitteePrvKeys
        , newCommitteePrvKeys: nextCommitteePrvKeys
        , previousMerkleRoot: Nothing
        , sidechainEpoch: BigInt.fromInt 1
        }
    $ \(UpdateCommitteeHashParams params) →
        pure
          $ UpdateCommitteeHashParams
          $ params
              { committeeSignatures =
                  Unsafe.unsafePartial
                    ( case params.committeeSignatures of
                        [ c1 /\ _s1
                        , c2 /\ s2
                        ] →
                          [ c1 /\ Nothing
                          , c2 /\ s2
                          ]
                    )
              }
