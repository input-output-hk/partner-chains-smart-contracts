module Test.CommitteeCandidateValidator
  ( testScenarioFailure1
  , testScenarioFailure2
  , testScenarioSuccess
  ) where

import Contract.Prelude

import CommitteCandidateValidator
  ( DeregisterParams(..)
  , RegisterParams(..)
  , deregister
  , register
  )
import Contract.Address (getWalletAddress)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Contract.Wallet (KeyWallet, withKeyWallet)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import SidechainParams (SidechainParams(..))
import Test.Utils (toTxIn)
import Utils.Crypto as Utils.Crypto

scParams ∷ SidechainParams
scParams = SidechainParams
  { chainId: BigInt.fromInt 69
  , genesisHash: hexToByteArrayUnsafe "112233"
  , genesisMint: Nothing
  , genesisUtxo: toTxIn
      "211307be24c471d42012c5ebd7d98c83f349c612023ce365f9fb5e3e758d0779"
      1
  , thresholdNumerator: BigInt.fromInt 2
  , thresholdDenominator: BigInt.fromInt 3
  }

mockSpoPubKey ∷ ByteArray
mockSpoPubKey = hexToByteArrayUnsafe
  "40802011e4fa2af0ec57dbf341cac38b344fe0867bfc67d38988dd1006d3eb9e"

runRegister ∷ Contract () Unit
runRegister = do
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "Cannot get UTxOs" (utxosAt ownAddr)
  registrationUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  void $ register $ RegisterParams
    { sidechainParams: scParams
    , spoPubKey: mockSpoPubKey
    , sidechainPubKey:
        Utils.Crypto.byteArrayToSidechainPublicKeyUnsafe
          $ hexToByteArrayUnsafe
              "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    , spoSig: hexToByteArrayUnsafe ""
    , sidechainSig:
        Utils.Crypto.byteArrayToSidechainSignatureUnsafe
          $ hexToByteArrayUnsafe
              "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    , inputUtxo: registrationUtxo
    }

runDeregister ∷ Contract () Unit
runDeregister =
  void $ deregister $ DeregisterParams
    { sidechainParams: scParams, spoPubKey: mockSpoPubKey }

-- Register then Deregister
testScenarioSuccess ∷ Contract () Unit
testScenarioSuccess = do
  runRegister
  runDeregister

-- Deregister without prior registeration (i.e. no registration utxo present)
testScenarioFailure1 ∷ Contract () Unit
testScenarioFailure1 = runDeregister

-- alice registers, bob deregisters. not allowed & should fail
testScenarioFailure2 ∷ KeyWallet → KeyWallet → Contract () Unit
testScenarioFailure2 alice bob = do
  withKeyWallet alice runRegister
  withKeyWallet bob runDeregister
