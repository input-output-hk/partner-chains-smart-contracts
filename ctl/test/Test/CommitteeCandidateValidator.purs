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
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Utxos (utxosAt)
import Contract.Wallet (KeyWallet, withKeyWallet)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import SidechainParams (SidechainParams(..))
import Test.Utils (toTxIn)

scParams ∷ SidechainParams
scParams = SidechainParams
  { chainId: BigInt.fromInt 1
  , genesisHash: hexToByteArrayUnsafe "aabbcc"
  , genesisMint: Nothing
  , genesisUtxo: toTxIn "aabbcc" 0
  , thresholdNumerator: BigInt.fromInt 2
  , thresholdDenominator: BigInt.fromInt 3
  }

runRegister ∷ Contract () Unit
runRegister = do
  ownAddr ← liftedM "Cannot get own address" getWalletAddress
  ownUtxos ← liftedM "Cannot get UTxOs" (utxosAt ownAddr)
  registrationUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  void $ register $ RegisterParams
    { sidechainParams: scParams
    , spoPubKey: hexToByteArrayUnsafe "ababab"
    , sidechainPubKey: hexToByteArrayUnsafe ""
    , spoSig: hexToByteArrayUnsafe ""
    , sidechainSig: hexToByteArrayUnsafe ""
    , inputUtxo: registrationUtxo
    }

runDeregister ∷ Contract () Unit
runDeregister =
  void $ deregister $ DeregisterParams
    { sidechainParams: scParams, spoPubKey: hexToByteArrayUnsafe "ababab" }

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
