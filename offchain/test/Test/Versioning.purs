module Test.Versioning (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Wallet as Wallet
import Data.List as List
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT)
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Unit.Assert (assert)
import Test.Utils
  ( WrappedTests
  , fails
  , getOwnTransactionInput
  , testnetGroup
  , withSingleMultiSig
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  )
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , initializeVersion
  ) as Versioning
import TrustlessSidechain.Versioning.Types (ScriptId(..))
import TrustlessSidechain.Versioning.Utils
  ( insertVersionLookupsAndConstraints
  , invalidateVersionLookupsAndConstraints
  ) as Versioning
import Type.Row (type (+))

-- | `tests` aggregate all the Versioning tests in one convenient function
tests ∷ WrappedTests
tests = testnetGroup "Minting and burning versioning tokens" $ do
  testInsertAndInvalidateSuccessScenario
  testInsertSameScriptTwiceSuccessScenario
  testInsertUnversionedScriptSuccessScenario
  testRemovingTwiceSameScriptFailScenario

-- | We insert a new version of a script, and invalidate the old one.
testInsertAndInvalidateSuccessScenario ∷ TestnetTest
testInsertAndInvalidateSuccessScenario =
  Mote.Monad.test "Inserting new version, then invalidate the old one"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) $ do
        pkh ← getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo ← getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          -- No versioned scripts are inserted.
          -- Assert that the actual versioned scripts set is empty
          assertNumberOfActualVersionedScripts sidechainParams 1 0 0
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0

          -- Insert all initial versioned scripts
          void $ initTokensMint sidechainParams 1
          void $ Versioning.initializeVersion sidechainParams 1
          -- void $ initNativeTokenMgmt sidechainParams 1
          -- This policy was already inserted by 'initSidechain'.

          committeeCandidateValidator ←
            getCommitteeCandidateValidator sidechainParams

          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                2
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            >>=
              balanceSignAndSubmit "Test: insert new version of policy"

          void
            $ Versioning.invalidateVersionLookupsAndConstraints
                sidechainParams
                1
                CommitteeCandidateValidator
            >>=
              balanceSignAndSubmit "Test: invalidate old version of policy"

          -- At this point we expect the following policies to be inserted
          -- into the versioning system by the call to `initSidechain`:
          --
          --   * ReserveAuthPolicy
          --   * GovernancePolicy
          --
          -- We also expect the following validators:
          --
          --   * CommitteeCandidateValidator
          --   * ReserveValidator
          --   * IlliquidCirculationSupplyValidator
          --
          -- Note that we have defined a multisig governance for this test,
          -- which means that ReserveAuthPolicy and GovernancePolicy policies,
          -- as well as ReserveValidator and IlliquidCirculationSupplyValidator
          -- get inserted.
          assertNumberOfActualVersionedScripts sidechainParams 1 2 2
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0

-- | We insert the same script (same ScriptId and same version) twice. That
-- should work.
testInsertSameScriptTwiceSuccessScenario ∷ TestnetTest
testInsertSameScriptTwiceSuccessScenario =
  Mote.Monad.test "Insert same script with the same version twice"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo ← getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          assertNumberOfActualVersionedScripts sidechainParams 1 0 0
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0

          void $ initTokensMint sidechainParams 1
          void $ Versioning.initializeVersion sidechainParams 1

          committeeCandidateValidator ←
            getCommitteeCandidateValidator sidechainParams

          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                1
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            >>=
              balanceSignAndSubmit "Test: insert versioned policy"

          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                1
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            >>=
              balanceSignAndSubmit "Test: insert the same version of policy"

          assertNumberOfActualVersionedScripts sidechainParams 1 2 5
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0

-- | We insert an script that is not part of the initial versioned scripts.
testInsertUnversionedScriptSuccessScenario ∷ TestnetTest
testInsertUnversionedScriptSuccessScenario =
  Mote.Monad.test "Insert an unversioned script"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo ← getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          assertNumberOfActualVersionedScripts sidechainParams 1 0 0
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0

          void $ initTokensMint sidechainParams 1
          void $ Versioning.initializeVersion sidechainParams 1

          { dParameterMintingPolicy } ←
            getDParameterMintingPolicyAndCurrencySymbol sidechainParams
          assertNumberOfActualVersionedScripts sidechainParams 1 2 3
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0
          -- This validator is not part of the versioned scripts hardcoded list,
          -- so it should *not* be inserted.
          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                1
                (DParameterPolicy /\ dParameterMintingPolicy)
            >>=
              balanceSignAndSubmit "Test: insert non-versioned validator version"
          assertNumberOfActualVersionedScripts sidechainParams 1 2 3
          assertNumberOfActualVersionedScripts sidechainParams 2 0 0

-- | After inserting a versioned script, invalidating it twice should fail in the second
-- | invalidation call.
testRemovingTwiceSameScriptFailScenario ∷ TestnetTest
testRemovingTwiceSameScriptFailScenario =
  Mote.Monad.test "Removing the same script twice should fail"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo ← getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          void $ initTokensMint sidechainParams 1
          void $ Versioning.initializeVersion sidechainParams 1

          void
            $ Versioning.invalidateVersionLookupsAndConstraints
                sidechainParams
                1
                CommitteeCandidateValidator
            >>=
              balanceSignAndSubmit "Test: insert the same version of policy"

          ( void
              $ Versioning.invalidateVersionLookupsAndConstraints
                  sidechainParams
                  1
                  CommitteeCandidateValidator
              >>=
                balanceSignAndSubmit "Test: insert the same version of policy"
          )
            # withUnliftApp fails

assertNumberOfActualVersionedScripts ∷
  ∀ r.
  SidechainParams →
  -- | Version number
  Int →
  -- | Number of expected versionned minting policy scripts
  Int →
  -- | Number of expected versionned validator scripts
  Int →
  Run
    (EXCEPT OffchainError + TRANSACTION + WALLET + READER Env + AFF + EFFECT + r)
    Unit
assertNumberOfActualVersionedScripts
  sidechainParams
  version
  numExpectedVersionedPolicies
  numExpectedVersionedValidators = do
  Versioning.getActualVersionedPoliciesAndValidators sidechainParams
    version >>=
    \{ versionedPolicies, versionedValidators } → do
      liftAff
        $ assert
            ( "`Expected to return `"
                <> show numExpectedVersionedPolicies
                <> "` versioned policies, and `"
                <> show numExpectedVersionedValidators
                <> "` versioned validators, but got `"
                <> show (List.length versionedPolicies)
                <> "` versioned policies and `"
                <> show (List.length versionedValidators)
                <> "` versioned validators`."
            )
            ( List.length versionedPolicies == numExpectedVersionedPolicies
                && List.length versionedValidators
                == numExpectedVersionedValidators
            )
