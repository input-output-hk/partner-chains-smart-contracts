module Test.FUELMintingPolicy where

import Contract.Prelude

import Contract.Address (getWalletAddress, ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionInput)
import Contract.Utxos (utxosAt)
import Data.BigInt as BigInt
import Data.List.Lazy (List, replicateM)
import Data.Map as Map
import Data.Set as Set
import FUELMintingPolicy (FuelParams(..), passiveBridgeMintParams, runFuelMP)
import MerkleTree (MerkleProof(..), fromList, lookupMp)
import SidechainParams (SidechainParams(..))
import Test.Utils (toTxIn)
import Utils.Crypto (PrivateKey, PublicKey, generatePrivKey, toPubKeyUnsafe)
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

mkCommittee ∷ Int → Contract () (List (Tuple PublicKey PrivateKey))
mkCommittee n = replicateM n (Tuple <*> toPubKeyUnsafe <$> generatePrivKey)

-- | Testing Passive bridge minting (genesis mint) and burning multiple times
testScenarioPassiveSuccess ∷ Contract () Unit
testScenarioPassiveSuccess = do
  pk ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
  genesisMint ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  let scParams = mkScParams (Just genesisMint)
  void $ runFuelMP scParams $ passiveBridgeMintParams scParams
    { amount: BigInt.fromInt 5, recipient: pk }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 2, recipient: hexToByteArrayUnsafe "aabbcc" }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 3, recipient: hexToByteArrayUnsafe "aabbcc" }

-- | Testing multiple mints on passive bridge (should fail)
testScenarioPassiveFailure ∷ Contract () Unit
testScenarioPassiveFailure = do
  pk ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "cannot get UTxOs" (utxosAt ownAddr)
  genesisMint ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  let scParams = mkScParams (Just genesisMint)
  void $ runFuelMP scParams $ passiveBridgeMintParams scParams
    { amount: BigInt.fromInt 5, recipient: pk }
  void $ runFuelMP scParams $ passiveBridgeMintParams scParams
    { amount: BigInt.fromInt 5, recipient: pk }

testScenarioActiveSuccess ∷ Contract () Unit
testScenarioActiveSuccess = do
  pk ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  let
    scParams = mkScParams Nothing
  -- TODO: This is not how you create a working merkleproof that passes onchain validator..
  mp' ← liftedM "impossible" $ pure (serialiseData (toData (MerkleProof [])))
  mt ← liftedE $ pure (fromList (pure mp'))
  mp ← liftedM "couldn't lookup merkleproof" $ pure (lookupMp mp' mt)

  void $ runFuelMP scParams $ Mint
    { merkleProof: mp
    , recipient: pk
    , sidechainParams: scParams
    , amount: BigInt.fromInt 5
    , index: BigInt.fromInt 0
    , previousMerkleRoot: Nothing -- Just $ byteArrayFromIntArrayUnsafe (replicate 32 0)
    }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 2, recipient: hexToByteArrayUnsafe "aabbcc" }
  void $ runFuelMP scParams $ Burn
    { amount: BigInt.fromInt 3, recipient: hexToByteArrayUnsafe "aabbcc" }

testScenarioActiveFailure ∷ Contract () Unit
testScenarioActiveFailure = liftedM "unemplemented" (pure Nothing)
