module Test.CommitteeCandidateValidator
  ( tests
  , runRegisterWithCandidatePermissionInfo
  , runDeregister
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash)
import Contract.Utxos (utxosAt)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.List.Lazy (replicate)
import Data.Map as Map
import Data.Set as Set
import Effect.Random (randomInt)
import Mote.Monad as Mote.Monad
import Run (EFFECT, Run)
import Run (liftEffect) as Run
import Run.Except (EXCEPT)
import Run.Except (note) as Run
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, dummySidechainParams, fails, testnetGroup)
import TrustlessSidechain.CommitteeCandidateValidator
  ( DeregisterParams(DeregisterParams)
  , RegisterParams(RegisterParams)
  , StakeOwnership(..)
  , deregister
  , register
  )
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnWalletAddress)
import Type.Row (type (+))

mockSpoPubKey ∷ ByteArray
mockSpoPubKey = hexToByteArrayUnsafe
  "40802011e4fa2af0ec57dbf341cac38b344fe0867bfc67d38988dd1006d3eb9e"

-- | `tests` wraps up all the committee candidate validator tests conveniently
tests ∷ WrappedTests
tests = testnetGroup "Committe candidate registration/deregistration" $ do
  testScenarioSuccess1
  testScenarioSuccess2
  testScenarioFailure1
  testScenarioFailure2
  testScenarioFailure3

-- | `runRegister` runs the register endpoint without any candidate permission
-- | information.
runRegister ∷
  ∀ r.
  SidechainParams →
  Run
    ( READER Env + EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT
        + EFFECT
        + r
    )
    TransactionHash
runRegister = runRegisterWithCandidatePermissionInfo false

-- | `runRegister` runs the register endpoint without any candidate permission
-- | information.
runRegisterWithFixedKeys ∷
  ∀ r.
  SidechainParams →
  Run
    ( READER Env + EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT +
        r
    )
    TransactionHash
runRegisterWithFixedKeys =
  runRegisterWithCandidatePermissionInfoWithFixedKeys false

runRegisterWithCandidatePermissionInfoWithFixedKeys ∷
  ∀ r.
  Boolean →
  SidechainParams →
  Run
    ( READER Env + EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT +
        r
    )
    TransactionHash
runRegisterWithCandidatePermissionInfoWithFixedKeys usePermissionToken scParams =
  do
    ownAddr ← getOwnWalletAddress
    ownUtxos ← liftContract $ utxosAt ownAddr
    registrationUtxo ←
      Run.note (GenericInternalError "No UTxOs found at key wallet")
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
      , usePermissionToken
      , auraKey: hexToByteArrayUnsafe
          "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
      , grandpaKey: hexToByteArrayUnsafe
          "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
      }

runRegisterWithCandidatePermissionInfo ∷
  ∀ r.
  Boolean →
  SidechainParams →
  Run
    ( READER Env + EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT
        + EFFECT
        + r
    )
    TransactionHash
runRegisterWithCandidatePermissionInfo usePermissionToken scParams = do
  let
    generateKey = byteArrayFromIntArrayUnsafe <$>
      (sequence $ Array.replicate 32 (randomInt 0 255))
  ownAddr ← getOwnWalletAddress
  ownUtxos ← liftContract $ utxosAt ownAddr
  registrationUtxo ←
    Run.note (GenericInternalError "No UTxOs found at key wallet")
      $ Set.findMin
      $ Map.keys ownUtxos

  -- we generate only aura and grandpa keys. This will be enough to mark this
  -- candidate as a new one, and at the same time it doesn't require us to
  -- generate correct sidechainSig
  auraKey ← Run.liftEffect generateKey
  grandpaKey ← Run.liftEffect generateKey

  register $ RegisterParams
    { sidechainParams: scParams
    , stakeOwnership: AdaBasedStaking mockSpoPubKey (hexToByteArrayUnsafe "")
    , sidechainPubKey: hexToByteArrayUnsafe
        "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    , sidechainSig: hexToByteArrayUnsafe
        "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    , inputUtxo: registrationUtxo
    , usePermissionToken
    , auraKey
    , grandpaKey
    }

runDeregister ∷
  ∀ r.
  SidechainParams →
  Run (READER Env + EXCEPT OffchainError + WALLET + TRANSACTION + LOG + r) Unit
runDeregister scParams =
  void $ deregister $ DeregisterParams
    { sidechainParams: scParams, spoPubKey: Just mockSpoPubKey }

-- Register then Deregister
testScenarioSuccess1 ∷ TestnetTest
testScenarioSuccess1 = Mote.Monad.test "Register followed by deregister"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      void $ runRegister dummySidechainParams
      runDeregister dummySidechainParams

-- Register multipe times then Deregister
testScenarioSuccess2 ∷ TestnetTest
testScenarioSuccess2 =
  Mote.Monad.test "10 registrations followed by 1 deregister"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 5_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sequence_ $ replicate 10 $ runRegister dummySidechainParams
        runDeregister dummySidechainParams

-- Deregister without prior registeration (i.e. no registration utxo present)
testScenarioFailure1 ∷ TestnetTest
testScenarioFailure1 = Mote.Monad.test "Deregister in isolation (should fail)"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      runDeregister dummySidechainParams # withUnliftApp fails

-- alice registers, bob deregisters. not allowed & should fail
testScenarioFailure2 ∷ TestnetTest
testScenarioFailure2 =
  Mote.Monad.test
    "Register followed by a deregister from a distinct wallet (should fail)"
    $ Test.TestnetTest.mkTestnetConfigTest
        ( [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ] /\
            [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ]
        )
    $ \(alice /\ bob) →
        do
          withUnliftApp (Wallet.withKeyWallet alice) $ void $ runRegister
            dummySidechainParams
          withUnliftApp (Wallet.withKeyWallet bob) $ runDeregister
            dummySidechainParams
          # withUnliftApp fails

-- alice registers, then tries to register again with the same set of keys. not allowed & should fail
testScenarioFailure3 ∷ TestnetTest
testScenarioFailure3 =
  Mote.Monad.test
    "Register followed by a register with the same set of keys (should fail)"
    $ Test.TestnetTest.mkTestnetConfigTest
        ( [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ]
        )
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) $ do
        void $ runRegisterWithFixedKeys dummySidechainParams
        (void $ runRegisterWithFixedKeys dummySidechainParams)
          # withUnliftApp fails
