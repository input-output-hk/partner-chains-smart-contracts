module Test.FUELMintingPolicy where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , pubKeyHashAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionInput)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Ctl.Internal.Serialization.Address (addressBytes)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.List.Lazy (List, replicateM)
import Data.Map as Map
import Data.Set as Set
import FUELMintingPolicy
  ( FuelParams(..)
  , MerkleTreeEntry(..)
  , passiveBridgeMintParams
  , runFuelMP
  )
import InitSidechain (initSidechain)
import MerkleTree (MerkleProof(..), fromList, lookupMp)
import MerkleTree as MerkleTree
import Partial.Unsafe (unsafePartial)
import SidechainParams (InitSidechainParams(..), SidechainParams(..))
import Test.MPTRoot as Test.MPTRoot
import Test.Utils (getOwnTransactionInput, toTxIn)
import Utils.Crypto
  ( SidechainPrivateKey
  , SidechainPublicKey
  , generatePrivKey
  , toPubKeyUnsafe
  )
import Utils.SerialiseData (serialiseData)

mkScParams ∷ Maybe TransactionInput → SidechainParams
mkScParams genesisMint = SidechainParams
  { chainId: BigInt.fromInt 1
  , genesisHash: hexToByteArrayUnsafe "aabbcc"
  , genesisUtxo: toTxIn "aabbcc" 0
  , thresholdNumerator: BigInt.fromInt 2
  , thresholdDenominator: BigInt.fromInt 3
  , genesisMint
  }

mkCommittee ∷
  Int → Contract () (List (Tuple SidechainPublicKey SidechainPrivateKey))
mkCommittee n = replicateM n ado
  prvKey ← generatePrivKey
  in (toPubKeyUnsafe prvKey) /\ prvKey

-- | Testing Passive bridge minting (genesis mint) and burning multiple times
testScenarioPassiveSuccess ∷ Contract () Unit
testScenarioPassiveSuccess = do
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
  genesisMint ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  let
    scParams = mkScParams (Just genesisMint)
    recipient = pubKeyHashAddress pkh Nothing

  void $ runFuelMP scParams $ passiveBridgeMintParams scParams
    { amount: BigInt.fromInt 5, recipient }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 2, recipient: hexToByteArrayUnsafe "aabbcc" }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 3, recipient: hexToByteArrayUnsafe "aabbcc" }

-- | Testing multiple mints on passive bridge (should fail)
testScenarioPassiveFailure ∷ Contract () Unit
testScenarioPassiveFailure = do
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
  genesisMint ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  let
    scParams = mkScParams (Just genesisMint)
    recipient = pubKeyHashAddress pkh Nothing
  void $ runFuelMP scParams $ passiveBridgeMintParams scParams
    { amount: BigInt.fromInt 5, recipient }
  void $ runFuelMP scParams $ passiveBridgeMintParams scParams
    { amount: BigInt.fromInt 5, recipient }

testScenarioActiveSuccess ∷ Contract () Unit
testScenarioActiveSuccess = do
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  netId ← getNetworkId
  genesisUtxo ← getOwnTransactionInput
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
        , recipient: unwrap (addressBytes (fromPlutusAddress netId recipient))
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

testScenarioActiveFailure ∷ Contract () Unit
testScenarioActiveFailure = do
  pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  let
    recipient = pubKeyHashAddress pkh Nothing
    scParams = mkScParams Nothing
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
