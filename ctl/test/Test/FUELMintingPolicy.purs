module Test.FUELMintingPolicy where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash, pubKeyHashAddress)
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  )
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Data.Array as Array
import Data.BigInt as BigInt
import FUELMintingPolicy
  ( FuelParams(..)
  , MerkleTreeEntry(..)
  , combinedMerkleProofToFuelParams
  , runFuelMP
  )
import InitSidechain (initSidechain)
import MerkleTree (MerkleProof(..), fromList, lookupMp)
import MerkleTree as MerkleTree
import Partial.Unsafe (unsafePartial)
import SidechainParams (InitSidechainParams(..), SidechainParams(..))
import Test.MPTRoot as Test.MPTRoot
import Test.Utils (assertMaxFee, getOwnTransactionInput, toTxIn)
import Utils.Crypto (generatePrivKey, toPubKeyUnsafe)
import Utils.SerialiseData (serialiseData)

-- | `testScenarioActiveSuccess` tets minting some tokens
testScenarioActiveSuccess ∷ Contract () Unit
testScenarioActiveSuccess = do
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownRecipient ← Test.MPTRoot.paymentPubKeyHashToBech32Bytes pkh
  genesisUtxo ← getOwnTransactionInput
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
    amount = BigInt.fromInt 5
    recipient = pubKeyHashAddress pkh Nothing
    index = BigInt.fromInt 0
    previousMerkleRoot = Nothing
    ownEntry =
      MerkleTreeEntry
        { index
        , amount
        , previousMerkleRoot
        , recipient: ownRecipient
        }

    ownEntryBytes = unsafePartial
      $ fromJust
      $ serialiseData
      $ toData ownEntry
    merkleTree =
      unsafePartial $ fromJust $ hush $ MerkleTree.fromArray
        [ ownEntryBytes ]

    merkleProof = unsafePartial $ fromJust $ MerkleTree.lookupMp ownEntryBytes
      merkleTree
  void $ Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries: [ ownEntry ]
    , currentCommitteePrvKeys: initCommitteePrvKeys
    , previousMerkleRoot: Nothing
    }

  void $ runFuelMP sidechainParams
    ( Mint
        { amount
        , recipient
        , sidechainParams
        , merkleProof
        , index
        , previousMerkleRoot
        }
    )

-- | `testScenarioActiveSuccess2` mints and burns a few times.. In particular, we:
-- |    - mint 5
-- |    - mint 7
-- |    - burn 10
-- |    - burn 2
testScenarioActiveSuccess2 ∷ Contract () Unit
testScenarioActiveSuccess2 = do
  -- start of mostly duplicated code from `testScenarioActiveSuccess`
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownRecipient ← Test.MPTRoot.paymentPubKeyHashToBech32Bytes pkh
  genesisUtxo ← getOwnTransactionInput
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
  -- end of mostly duplicated code from `testScenarioActiveSuccess`

  { sidechainParams } ← initSidechain initScParams

  { combinedMerkleProofs } ← Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries:
        let
          previousMerkleRoot = Nothing
          entry0 =
            MerkleTreeEntry
              { index: BigInt.fromInt 0
              , amount: BigInt.fromInt 5
              , previousMerkleRoot
              , recipient: ownRecipient
              }
          entry1 =
            MerkleTreeEntry
              { index: BigInt.fromInt 1
              , amount: BigInt.fromInt 7
              , previousMerkleRoot
              , recipient: ownRecipient
              }
        in
          [ entry0, entry1 ]
    , currentCommitteePrvKeys: initCommitteePrvKeys
    , previousMerkleRoot: Nothing
    }

  (combinedMerkleProof0 /\ combinedMerkleProof1) ←
    liftContractM "bad test case for `testScenarioActiveSuccess2`"
      $ case combinedMerkleProofs of
          [ combinedMerkleProof0, combinedMerkleProof1 ] → pure
            $ combinedMerkleProof0
            /\ combinedMerkleProof1
          _ → Nothing

  fp0 ←
    liftContractM
      "`Test.FUELMintingPolicy.testScenarioActiveSuccess2` failed converting to FUELParams"
      $ combinedMerkleProofToFuelParams sidechainParams combinedMerkleProof0

  fp1 ←
    liftContractM
      "`Test.FUELMintingPolicy.testScenarioActiveSuccess2` failed converting to FUELParams"
      $ combinedMerkleProofToFuelParams sidechainParams combinedMerkleProof1

  assertMaxFee (BigInt.fromInt 1_700_000) =<< runFuelMP sidechainParams fp0
  assertMaxFee (BigInt.fromInt 1_700_000) =<< runFuelMP sidechainParams fp1

  assertMaxFee (BigInt.fromInt 600_000) =<< runFuelMP sidechainParams
    ( Burn
        { amount: BigInt.fromInt 10, recipient: hexToByteArrayUnsafe "aabbcc" }
    )

  assertMaxFee (BigInt.fromInt 600_000) =<< runFuelMP sidechainParams
    (Burn { amount: BigInt.fromInt 2, recipient: hexToByteArrayUnsafe "aabbcc" })

  pure unit

testScenarioActiveFailure ∷ Contract () Unit
testScenarioActiveFailure = do
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  let
    recipient = pubKeyHashAddress pkh Nothing
    scParams = SidechainParams
      { chainId: BigInt.fromInt 1
      , genesisHash: hexToByteArrayUnsafe "aabbcc"
      , genesisUtxo: toTxIn "aabbcc" 0
      , thresholdNumerator: BigInt.fromInt 2
      , thresholdDenominator: BigInt.fromInt 3
      }

  -- This is not how you create a working merkleproof that passes onchain validator.
  mp' ← liftedM "impossible" $ pure (serialiseData (toData (MerkleProof [])))
  mt ← liftedE $ pure (fromList (pure mp'))
  mp ← liftedM "couldn't lookup merkleproof" $ pure (lookupMp mp' mt)

  void $ runFuelMP scParams $ Mint
    { merkleProof: mp
    , recipient
    , sidechainParams: scParams
    , amount: BigInt.fromInt 1
    , index: BigInt.fromInt 0
    , previousMerkleRoot: Nothing -- Just $ byteArrayFromIntArrayUnsafe (replicate 32 0)
    }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 1, recipient: hexToByteArrayUnsafe "aabbcc" }

-- | `testScenarioActiveFailure2` tries to mint something twice (which should
-- | fail!)
testScenarioActiveFailure2 ∷ Contract () Unit
testScenarioActiveFailure2 = do
  -- start of mostly duplicated code from `testScenarioActiveSuccess2`
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownRecipient ← Test.MPTRoot.paymentPubKeyHashToBech32Bytes pkh
  genesisUtxo ← getOwnTransactionInput
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

  { combinedMerkleProofs } ← Test.MPTRoot.saveRoot
    { sidechainParams
    , merkleTreeEntries:
        let
          previousMerkleRoot = Nothing
          entry0 =
            MerkleTreeEntry
              { index: BigInt.fromInt 0
              , amount: BigInt.fromInt 5
              , previousMerkleRoot
              , recipient: ownRecipient
              }
          entry1 =
            MerkleTreeEntry
              { index: BigInt.fromInt 1
              , amount: BigInt.fromInt 7
              , previousMerkleRoot
              , recipient: ownRecipient
              }
        in
          [ entry0, entry1 ]
    , currentCommitteePrvKeys: initCommitteePrvKeys
    , previousMerkleRoot: Nothing
    }
  -- end of mostly duplicated code from `testScenarioActiveSuccess2`

  (combinedMerkleProof0 /\ _combinedMerkleProof1) ←
    liftContractM "bad test case for `testScenarioActiveSuccess2`"
      $ case combinedMerkleProofs of
          [ combinedMerkleProof0, combinedMerkleProof1 ] → pure
            $ combinedMerkleProof0
            /\ combinedMerkleProof1
          _ → Nothing

  fp0 ←
    liftContractM
      "`Test.FUELMintingPolicy.testScenarioActiveSuccess2` failed converting to FUELParams"
      $ combinedMerkleProofToFuelParams sidechainParams combinedMerkleProof0

  -- the very bad double mint attempt...
  void $ runFuelMP sidechainParams fp0
  void $ runFuelMP sidechainParams fp0

  pure unit
