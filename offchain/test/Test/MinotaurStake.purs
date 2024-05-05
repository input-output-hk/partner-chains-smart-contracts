module Test.MinotaurStake (tests) where

import Contract.Prelude

import Contract.Prim.ByteArray as ByteArray
import Contract.Wallet as Wallet
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Test.UtxoDistribution
  ( InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  )
import Ctl.Internal.Wallet.Key (PrivateStakeKey(PrivateStakeKey))
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.InitSidechain.Utils (failMsg)
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert (assert)
import Test.Utils (WrappedTests, fails, plutipGroup)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.MinotaurStake as MinotaurStake
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  )

-- | `tests` aggregate all the DParameterPolicy tests in one convenient
-- | function
tests ∷ PrivateKey → PrivateKey → WrappedTests
tests pk pk2 = plutipGroup "Minting and burning a Minotaur Stake Token" $
  do
    testScenarioSuccess pk
    testScenarioFailure
    testScenarioSuccessCancelDelegation pk
    testScenarioFailureCancelDelegation pk pk2

testScenarioSuccess ∷ PrivateKey → PlutipTest
testScenarioSuccess privateKey =
  Mote.Monad.test "Making Minotaur Stake Delegation"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( InitialUTxOsWithStakeKey (PrivateStakeKey privateKey)
            [ BigInt.fromInt 1_000_000
            , BigInt.fromInt 5_000_000
            , BigInt.fromInt 150_000_000
            , BigInt.fromInt 150_000_000
            ]
        )

    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        let stakePoolId = ByteArray.hexToByteArrayUnsafe "abababababa"
        _ ←
          ( MinotaurStake.mkMinotaurDelegateLookupsAndConstraints
              { stakePoolId
              , partnerChainRewardAddress: ByteArray.hexToByteArrayUnsafe
                  "abababababa"
              }
              >>=
                balanceSignAndSubmit
                  "Test: delegate minotaur stake"
          )
        ownDelegations ← MinotaurStake.getOwnMinotaurDelegations
        Effect.fromMaybeThrow (GenericInternalError "Unreachable")
          $ map Just
          $ liftAff
          $ assert (failMsg "Own delegation was not found" ownDelegations)
              (length ownDelegations == 1)

        stakePoolDelegations ←
          MinotaurStake.getMinotaurDelegationsForGivenStakePoolId
            { stakePoolId }

        Effect.fromMaybeThrow (GenericInternalError "Unreachable")
          $ map Just
          $ liftAff
          $ assert
              (failMsg "Stake pool delegation was not found" stakePoolDelegations)
              (length stakePoolDelegations == 1)

testScenarioFailure ∷ PlutipTest
testScenarioFailure =
  Mote.Monad.test
    "Making Minotaur Stake Delegation without a stake key (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 1_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 150_000_000
        , BigInt.fromInt 150_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do

          let stakePoolId = ByteArray.hexToByteArrayUnsafe "abababababa"
          _ ←
            ( MinotaurStake.mkMinotaurDelegateLookupsAndConstraints
                { stakePoolId
                , partnerChainRewardAddress: ByteArray.hexToByteArrayUnsafe
                    "abababababa"
                }
                >>=
                  balanceSignAndSubmit
                    "Test: delegate minotaur stake"
            )
          pure unit
          # withUnliftApp fails

testScenarioSuccessCancelDelegation ∷ PrivateKey → PlutipTest
testScenarioSuccessCancelDelegation privateKey =
  Mote.Monad.test "Minting then burning a Minotaur Stake Token"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( InitialUTxOsWithStakeKey (PrivateStakeKey privateKey)
            [ BigInt.fromInt 1_000_000
            , BigInt.fromInt 5_000_000
            , BigInt.fromInt 150_000_000
            , BigInt.fromInt 150_000_000
            ]
        )

    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        let
          partnerChainRewardAddress = ByteArray.hexToByteArrayUnsafe
            "abababababa"
          stakePoolId = ByteArray.hexToByteArrayUnsafe "abababababa"

        _ ←
          ( MinotaurStake.mkMinotaurDelegateLookupsAndConstraints
              { stakePoolId, partnerChainRewardAddress }
              >>=
                balanceSignAndSubmit
                  "Test: delegate minotaur stake"
          )

        _ ←
          ( MinotaurStake.mkMinotaurCancelDelegationLookupsAndConstraints
              { stakePoolId, partnerChainRewardAddress }
              >>=
                balanceSignAndSubmit
                  "Test: cancel delegation of minotaur stake"
          )

        ownDelegations ← MinotaurStake.getOwnMinotaurDelegations
        Effect.fromMaybeThrow (GenericInternalError "Unreachable")
          $ map Just
          $ liftAff
          $ assert (failMsg "Own delegation removed" ownDelegations)
              (length ownDelegations == 0)

testScenarioFailureCancelDelegation ∷ PrivateKey → PrivateKey → PlutipTest
testScenarioFailureCancelDelegation privateKey privateKey2 =
  Mote.Monad.test
    "Delegating, then trying to cancel delegation using different wallet (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( InitialUTxOsWithStakeKey (PrivateStakeKey privateKey)
            [ BigInt.fromInt 1_000_000
            , BigInt.fromInt 5_000_000
            , BigInt.fromInt 150_000_000
            , BigInt.fromInt 150_000_000
            ]
            /\ InitialUTxOsWithStakeKey (PrivateStakeKey privateKey2)
              [ BigInt.fromInt 1_000_000
              , BigInt.fromInt 5_000_000
              , BigInt.fromInt 150_000_000
              , BigInt.fromInt 150_000_000
              ]
        )

    $ \(alice /\ bob) → do

        withUnliftApp (Wallet.withKeyWallet alice) do
          let
            partnerChainRewardAddress = ByteArray.hexToByteArrayUnsafe
              "abababababa"
            stakePoolId = ByteArray.hexToByteArrayUnsafe "abababababa"

          _ ←
            ( MinotaurStake.mkMinotaurDelegateLookupsAndConstraints
                { stakePoolId, partnerChainRewardAddress }
                >>=
                  balanceSignAndSubmit
                    "Test: delegate minotaur stake"
            )
          pure unit

        withUnliftApp (Wallet.withKeyWallet bob) do
          let
            partnerChainRewardAddress = ByteArray.hexToByteArrayUnsafe
              "abababababa"
            stakePoolId = ByteArray.hexToByteArrayUnsafe "abababababa"
          _ ←
            ( MinotaurStake.mkMinotaurCancelDelegationLookupsAndConstraints
                { stakePoolId, partnerChainRewardAddress }
                >>=
                  balanceSignAndSubmit
                    "Test: cancel delegation of minotaur stake"
            )
          pure unit
          # withUnliftApp fails
