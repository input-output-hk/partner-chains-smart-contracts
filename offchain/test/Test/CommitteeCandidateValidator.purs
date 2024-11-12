module Test.CommitteeCandidateValidator
  ( suite
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
import Contract.Test.Testnet (withWallets)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.Utxos (utxosAt)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.List.Lazy (replicate)
import Data.Map as Map
import Data.Set as Set
import Effect.Random (randomInt)
import Mote.Monad (group, test)
import Run (EFFECT, Run)
import Run (liftEffect) as Run
import Run.Except (EXCEPT)
import Run.Except (note) as Run
import Test.Utils (TestnetTest, dummyGenesisUtxo, fails)
import TrustlessSidechain.CommitteeCandidateValidator
  ( DeregisterParams(DeregisterParams)
  , RegisterParams(RegisterParams)
  , StakeOwnership(..)
  , deregister
  , register
  )
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (unliftApp, withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Utils.Address (getOwnWalletAddress)
import Type.Row (type (+))

mockSpoPubKey :: ByteArray
mockSpoPubKey = hexToByteArrayUnsafe
  "40802011e4fa2af0ec57dbf341cac38b344fe0867bfc67d38988dd1006d3eb9e"

suite :: TestnetTest
suite = group "Committe candidate registration/deregistration" $ do
  testScenarioSuccess1
  testScenarioFailure1

-- | `runRegister` runs the register endpoint without any candidate permission
-- | information.
runRegister ::
  forall r.
  TransactionInput ->
  Run
    ( EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT
        + EFFECT
        + r
    )
    TransactionHash
runRegister = runRegisterWithCandidatePermissionInfo

runRegisterWithFixedKeys ::
  forall r.
  TransactionInput ->
  Run
    ( EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT +
        r
    )
    TransactionHash
runRegisterWithFixedKeys scParams =
  do
    ownAddr <- getOwnWalletAddress
    ownUtxos <- liftContract $ utxosAt ownAddr
    registrationUtxo <-
      Run.note (GenericInternalError "No UTxOs found at key wallet")
        $ Set.findMin
        $ Map.keys ownUtxos
    register $ RegisterParams
      { genesisUtxo: scParams
      , stakeOwnership: AdaBasedStaking mockSpoPubKey (hexToByteArrayUnsafe "")
      , sidechainPubKey: hexToByteArrayUnsafe
          "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
      , sidechainSig: hexToByteArrayUnsafe
          "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
      , inputUtxo: registrationUtxo
      , auraKey: hexToByteArrayUnsafe
          "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
      , grandpaKey: hexToByteArrayUnsafe
          "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
      }

runRegisterWithCandidatePermissionInfo ::
  forall r.
  TransactionInput ->
  Run
    ( EXCEPT OffchainError + LOG + TRANSACTION + WALLET + CONTRACT
        + EFFECT
        + r
    )
    TransactionHash
runRegisterWithCandidatePermissionInfo scParams = do
  let
    generateKey = byteArrayFromIntArrayUnsafe <$>
      (sequence $ Array.replicate 32 (randomInt 0 255))
  ownAddr <- getOwnWalletAddress
  ownUtxos <- liftContract $ utxosAt ownAddr
  registrationUtxo <-
    Run.note (GenericInternalError "No UTxOs found at key wallet")
      $ Set.findMin
      $ Map.keys ownUtxos

  -- we generate only aura and grandpa keys. This will be enough to mark this
  -- candidate as a new one, and at the same time it doesn't require us to
  -- generate correct sidechainSig
  auraKey <- Run.liftEffect generateKey
  grandpaKey <- Run.liftEffect generateKey

  register $ RegisterParams
    { genesisUtxo: scParams
    , stakeOwnership: AdaBasedStaking mockSpoPubKey (hexToByteArrayUnsafe "")
    , sidechainPubKey: hexToByteArrayUnsafe
        "02a4ee86ede04284ca75be10e08536d8772e66a80f654c3880659fb4143f716fc6"
    , sidechainSig: hexToByteArrayUnsafe
        "1f14b8e3d2291cdf11c8b77b63bc20cab2f0ed106f49a7282bc92da08cb90b0c56a8e667fcde29af358e1df55f75e9118c465041dcadeec0b89d5661dca4dbf3"
    , inputUtxo: registrationUtxo
    , auraKey
    , grandpaKey
    }

runDeregister ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + LOG + r) Unit
runDeregister scParams =
  void $ deregister $ DeregisterParams
    { genesisUtxo: scParams, spoPubKey: Just mockSpoPubKey }

-- Register multipe times then Deregister
testScenarioSuccess1 :: TestnetTest
testScenarioSuccess1 =
  test "10 registrations followed by 1 deregister" do
    let
      initialDistribution =
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 5_000_000
        , BigNum.fromInt 5_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp do
        sequence_ $ replicate 10 $ runRegister dummyGenesisUtxo
        runDeregister dummyGenesisUtxo

-- alice registers, bob deregisters. not allowed & should fail
-- also, alice tries to register again with the same set of keys. not allowed & should fail
testScenarioFailure1 :: TestnetTest
testScenarioFailure1 =
  test "Register followed by a deregister from a distinct wallet (should fail)"
    do
      let
        initialDistribution =
          [ BigNum.fromInt 5_000_000, BigNum.fromInt 5_000_000 ]
      withWallets (initialDistribution /\ initialDistribution) \(alice /\ bob) ->
        do
          unliftApp do
            withUnliftApp (Wallet.withKeyWallet alice) do
              void $ runRegisterWithFixedKeys dummyGenesisUtxo

              -- alice tries to register again with the same set of keys. not allowed & should fail
              (void $ runRegisterWithFixedKeys dummyGenesisUtxo) #
                withUnliftApp fails

            withUnliftApp (Wallet.withKeyWallet bob) do
              (void $ runDeregister dummyGenesisUtxo) # withUnliftApp fails
