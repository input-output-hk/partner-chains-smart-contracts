module Test.CommitteeCandidateValidator
  ( tests
  , testScenarioFailure1
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
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import Mote.Monad as Mote.Monad
import SidechainParams (SidechainParams(..))
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (fails, toTxIn)
import Utils.Crypto as Utils.Crypto

scParams ∷ SidechainParams
scParams = SidechainParams
  { chainId: BigInt.fromInt 69
  , genesisHash: hexToByteArrayUnsafe "112233"
  , genesisUtxo: toTxIn
      "211307be24c471d42012c5ebd7d98c83f349c612023ce365f9fb5e3e758d0779"
      1
  , thresholdNumerator: BigInt.fromInt 2
  , thresholdDenominator: BigInt.fromInt 3
  }

mockSpoPubKey ∷ ByteArray
mockSpoPubKey = hexToByteArrayUnsafe
  "40802011e4fa2af0ec57dbf341cac38b344fe0867bfc67d38988dd1006d3eb9e"

-- | `tests` wraps up all the committee candidate validator tests conveniently
tests ∷ PlutipTest
tests = Mote.Monad.group "Committe candidate registration/deregistration" $ do
  testScenarioSuccess
  testScenarioFailure1
  testScenarioFailure2

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
testScenarioSuccess ∷ PlutipTest
testScenarioSuccess = Mote.Monad.test "Register followed by deregister"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      runRegister
      runDeregister

-- Deregister without prior registeration (i.e. no registration utxo present)
testScenarioFailure1 ∷ PlutipTest
testScenarioFailure1 = Mote.Monad.test "Deregister in isolation (should fail)"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      runDeregister # fails

-- alice registers, bob deregisters. not allowed & should fail
testScenarioFailure2 ∷ PlutipTest
testScenarioFailure2 =
  Mote.Monad.test
    "Register followed by a deregister from a distinct wallet (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ] /\
            [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
        )
    $ \(alice /\ bob) →
        do
          Wallet.withKeyWallet alice runRegister
          Wallet.withKeyWallet bob runDeregister
          # fails
