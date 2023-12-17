module Test.CommitteeCandidateValidator
  ( tests
  , runRegisterWithCandidatePermissionInfo
  , runDeregister
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash)
import Contract.Utxos (utxosAt)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.List.Lazy (replicate)
import Data.Map as Map
import Data.Set as Set
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, dummySidechainParams, fails, plutipGroup)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenInfo
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( DeregisterParams(DeregisterParams)
  , RegisterParams(RegisterParams)
  , StakeOwnership(..)
  , deregister
  , register
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnWalletAddress)

mockSpoPubKey ∷ ByteArray
mockSpoPubKey = hexToByteArrayUnsafe
  "40802011e4fa2af0ec57dbf341cac38b344fe0867bfc67d38988dd1006d3eb9e"

-- | `tests` wraps up all the committee candidate validator tests conveniently
tests ∷ WrappedTests
tests = plutipGroup "Committe candidate registration/deregistration" $ do
  testScenarioSuccess1
  testScenarioSuccess2
  testScenarioFailure1
  testScenarioFailure2
  testScenarioFailure3

-- | `runRegister` runs the register endpoint without any candidate permission
-- | information.
runRegister ∷ SidechainParams → Contract TransactionHash
runRegister = runRegisterWithCandidatePermissionInfo Nothing

-- | `runRegister` runs the register endpoint without any candidate permission
-- | information.
runRegisterWithFixedKeys ∷ SidechainParams → Contract TransactionHash
runRegisterWithFixedKeys = runRegisterWithCandidatePermissionInfoWithFixedKeys
  Nothing

runRegisterWithCandidatePermissionInfoWithFixedKeys ∷
  Maybe CandidatePermissionTokenInfo →
  SidechainParams →
  Contract TransactionHash
runRegisterWithCandidatePermissionInfoWithFixedKeys cpti scParams = do
  ownAddr ← getOwnWalletAddress
  ownUtxos ← utxosAt ownAddr
  registrationUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos
  register $ RegisterParams
    { sidechainParams: scParams
    , stakeOwnership: AdaBasedStaking mockSpoPubKey (hexToByteArrayUnsafe "")
    , sidechainPubKey: hexToByteArrayUnsafe
        "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    , sidechainSig: hexToByteArrayUnsafe
        "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    , inputUtxo: registrationUtxo
    , permissionToken: cpti
    , auraKey: hexToByteArrayUnsafe
        "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    , grandpaKey: hexToByteArrayUnsafe
        "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    }

runRegisterWithCandidatePermissionInfo ∷
  Maybe CandidatePermissionTokenInfo →
  SidechainParams →
  Contract TransactionHash
runRegisterWithCandidatePermissionInfo cpti scParams = do
  let
    generateKey = byteArrayFromIntArrayUnsafe <$>
      (sequence $ Array.replicate 32 (randomInt 0 255))
  ownAddr ← getOwnWalletAddress
  ownUtxos ← utxosAt ownAddr
  registrationUtxo ← liftContractM "No UTxOs found at key wallet"
    $ Set.findMin
    $ Map.keys ownUtxos

  -- we generate only aura and grandpa keys. This will be enough to mark this
  -- candidate as a new one, and at the same time it doesn't require us to
  -- generate correct sidechainSig
  auraKey ← liftEffect generateKey
  grandpaKey ← liftEffect generateKey

  register $ RegisterParams
    { sidechainParams: scParams
    , stakeOwnership: AdaBasedStaking mockSpoPubKey (hexToByteArrayUnsafe "")
    , sidechainPubKey: hexToByteArrayUnsafe
        "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    , sidechainSig: hexToByteArrayUnsafe
        "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    , inputUtxo: registrationUtxo
    , permissionToken: cpti
    , auraKey
    , grandpaKey
    }

runDeregister ∷ SidechainParams → Contract Unit
runDeregister scParams =
  void $ deregister $ DeregisterParams
    { sidechainParams: scParams, spoPubKey: Just mockSpoPubKey }

-- Register then Deregister
testScenarioSuccess1 ∷ PlutipTest
testScenarioSuccess1 = Mote.Monad.test "Register followed by deregister"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      void $ runRegister dummySidechainParams
      runDeregister dummySidechainParams

-- Register multipe times then Deregister
testScenarioSuccess2 ∷ PlutipTest
testScenarioSuccess2 =
  Mote.Monad.test "10 registrations followed by 1 deregister"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        sequence_ $ replicate 10 $ runRegister dummySidechainParams
        runDeregister dummySidechainParams

-- Deregister without prior registeration (i.e. no registration utxo present)
testScenarioFailure1 ∷ PlutipTest
testScenarioFailure1 = Mote.Monad.test "Deregister in isolation (should fail)"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      runDeregister dummySidechainParams # fails

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
          Wallet.withKeyWallet alice $ void $ runRegister dummySidechainParams
          Wallet.withKeyWallet bob $ runDeregister dummySidechainParams
          # fails

-- alice registers, then tries to register again with the same set of keys. not allowed & should fail
testScenarioFailure3 ∷ PlutipTest
testScenarioFailure3 =
  Mote.Monad.test
    "Register followed by a register with the same set of keys (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( [ BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000 ]
        )
    $ \alice → Wallet.withKeyWallet alice $ do
        void $ runRegisterWithFixedKeys dummySidechainParams
        (void $ runRegisterWithFixedKeys dummySidechainParams)
          # fails
