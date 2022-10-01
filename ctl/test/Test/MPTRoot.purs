module Test.MPTRoot (testScenario) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Address as Address
import Contract.Log as Log
import Contract.Monad (Contract, liftContractE, liftContractM, liftedM)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import FUELMintingPolicy (MerkleTreeEntry(MerkleTreeEntry))
import InitSidechain as InitSidechain
import MPTRoot (SaveRootParams(SaveRootParams))
import MPTRoot as MPTRoot
import MerkleTree as MerkleTree
import Serialization.Hash as Hash
import SidechainParams (InitSidechainParams(..))
import Utils.Crypto as Crypto
import Utils.SerialiseData as SerialiseData

-- | Coerces a 'PaymentPubKeyHash' to a 'ByteArray'. This is useful when making
-- the recipient for the 'MerkleTreeEntry'.
paymentPubKeyHashToByteArray ∷ PaymentPubKeyHash → ByteArray
paymentPubKeyHashToByteArray =
  unwrap <<< Hash.ed25519KeyHashToBytes <<< unwrap <<< unwrap

-- | 'testScenario' does
-- 1. Sets up the sidechain using thw 'InitSidechain.initSidechain' endpoint
--
-- 2. Creates a dummy merkle root to sign
--
-- 3. Saves that merkle root with the current committee using the 'MPTRoot.saveRoot' endpoint.
testScenario ∷ Contract () Unit
testScenario = do
  Log.logInfo' "MPTRoot testScenario"

  ownAddr ← liftedM
    "error 'testScenario': 'Contract.Address.getWalletAddress' failed"
    Address.getWalletAddress
  ownUtxos ← liftedM "error 'testScenario': 'Contract.Address.utxosAt' failed" $
    utxosAt ownAddr

  -- 1. Setting up the sidechain
  ---------------------------
  let
    committeeSize = 25
  -- It fails with ~50 (nondeterministically) with budget overspent
  -- I would really like to get this up to 101 as with the update
  -- committee hash endpoint! Some room for optimization is certainly
  -- a possibility..
  genesisUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  initCommitteePrvKeys ← sequence $ Array.replicate committeeSize
    Crypto.generatePrivKey
  let
    initCommitteePubKeys = map Crypto.toPubKeyUnsafe initCommitteePrvKeys
    initSidechainParams = InitSidechainParams
      { initChainId: BigInt.fromInt 69
      , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
      , initMint: Nothing
      , initUtxo: genesisUtxo
      , initCommittee: initCommitteePubKeys
      }

  sidechainParams ← InitSidechain.initSidechain initSidechainParams

  -- Building / saving the root that pays lots of FUEL to this wallet :)
  ----------------------------------------------------------------------
  ownPaymentPubKeyHash ← liftedM
    "error 'testScenario': 'Contract.Address.ownPaymentPubKeyHash' failed"
    Address.ownPaymentPubKeyHash

  serialisedEntries ←
    liftContractM "error 'testScenario': bad serialisation of merkle root" $
      traverse SerialiseData.serialiseToData
        [ MerkleTreeEntry
            { index: BigInt.fromInt 0
            , amount: BigInt.fromInt 69
            , sidechainEpoch: BigInt.fromInt 420
            , recipient: paymentPubKeyHashToByteArray ownPaymentPubKeyHash
            }
        ]
  merkleTree ← liftContractE $ MerkleTree.fromArray serialisedEntries

  let merkleRoot = MerkleTree.unRootHash $ MerkleTree.rootHash merkleTree

  MPTRoot.saveRoot $ SaveRootParams
    { sidechainParams
    , merkleRoot
    , signatures: Crypto.multiSign initCommitteePrvKeys merkleRoot
    , threshold: BigInt.fromInt committeeSize
    , committeePubKeys: Array.sort initCommitteePubKeys
    }

  pure unit
