module Test.Versioning (suite) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Test.Testnet (withWallets)
import Contract.Wallet (withKeyWallet)
import Data.List as List
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT)
import Test.Unit.Assert (assert)
import Test.Utils
  ( TestnetTest
  , fails
  , getOwnTransactionInput
  , withSingleMultiSig
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  )
import TrustlessSidechain.Effects.Env (Env, READER, emptyEnv)
import TrustlessSidechain.Effects.Run (unliftApp, withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.Governance (initGovernance)
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

suite :: TestnetTest
suite = group "Minting and burning versioning tokens" $ do
  testInsertAndInvalidateSuccessScenario
  testInsertSameScriptTwiceSuccessScenario
  testInsertUnversionedScriptSuccessScenario
  testRemovingTwiceSameScriptFailScenario

-- | We insert a new version of a script, and invalidate the old one.
testInsertAndInvalidateSuccessScenario :: TestnetTest
testInsertAndInvalidateSuccessScenario =
  test "Inserting new version, then invalidate the it" do
    let
      initialDistribution =
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do
        pkh <- getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo <- getOwnTransactionInput
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
          assertNumberOfActualVersionedScripts sidechainParams 0 0

          -- Insert all initial versioned scripts
          void $ initGovernance sidechainParams pkh
          void $ Versioning.initializeVersion sidechainParams
          -- void $ initNativeTokenMgmt sidechainParams 1
          -- This policy was already inserted by 'initSidechain'.

          committeeCandidateValidator <-
            getCommitteeCandidateValidator sidechainParams

          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            >>=
              balanceSignAndSubmit "Test: insert new version of policy"

          void
            $ Versioning.invalidateVersionLookupsAndConstraints
                sidechainParams
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
          assertNumberOfActualVersionedScripts sidechainParams 3 3

-- | We insert the same script (same ScriptId and same version) twice. That
-- should work.
testInsertSameScriptTwiceSuccessScenario :: TestnetTest
testInsertSameScriptTwiceSuccessScenario =
  test "Insert same script with the same version twice" do
    let
      initialDistribution =
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do
        pkh <- getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo <- getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          assertNumberOfActualVersionedScripts sidechainParams 0 0

          void $ initGovernance sidechainParams pkh
          void $ Versioning.initializeVersion sidechainParams

          committeeCandidateValidator <-
            getCommitteeCandidateValidator sidechainParams

          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            >>=
              balanceSignAndSubmit "Test: insert versioned policy"

          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            >>=
              balanceSignAndSubmit "Test: insert the same version of policy"

          assertNumberOfActualVersionedScripts sidechainParams 3 5

-- | We insert an script that is not part of the initial versioned scripts.
testInsertUnversionedScriptSuccessScenario :: TestnetTest
testInsertUnversionedScriptSuccessScenario =
  test "Insert an unversioned script" do
    let
      initialDistribution =
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do
        pkh <- getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo <- getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          assertNumberOfActualVersionedScripts sidechainParams 0 0

          void $ initGovernance sidechainParams pkh
          void $ Versioning.initializeVersion sidechainParams

          { dParameterMintingPolicy } <-
            getDParameterMintingPolicyAndCurrencySymbol sidechainParams
          assertNumberOfActualVersionedScripts sidechainParams 3 3
          -- This validator is not part of the versioned scripts hardcoded list,
          -- so it should *not* be inserted.
          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                (DParameterPolicy /\ dParameterMintingPolicy)
            >>=
              balanceSignAndSubmit "Test: insert non-versioned validator version"
          assertNumberOfActualVersionedScripts sidechainParams 3 3

-- | After inserting a versioned script, invalidating it twice should fail in the second
-- | invalidation call.
testRemovingTwiceSameScriptFailScenario :: TestnetTest
testRemovingTwiceSameScriptFailScenario =
  test "Removing the same script twice should fail" do
    let
      initialDistribution =
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do
        pkh <- getOwnPaymentPubKeyHash
        withSingleMultiSig (unwrap pkh) $ do
          genesisUtxo <- getOwnTransactionInput
          let
            sidechainParams =
              SidechainParams
                { chainId: BigInt.fromInt 1
                , genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: Governance.mkGovernanceAuthority pkh
                }

          void $ initGovernance sidechainParams pkh
          void $ Versioning.initializeVersion sidechainParams

          void
            $ Versioning.invalidateVersionLookupsAndConstraints
                sidechainParams
                CommitteeCandidateValidator
            >>=
              balanceSignAndSubmit "Test: insert the same version of policy"

          ( void
              $ Versioning.invalidateVersionLookupsAndConstraints
                  sidechainParams
                  CommitteeCandidateValidator
              >>=
                balanceSignAndSubmit "Test: insert the same version of policy"
          )
            # withUnliftApp fails

assertNumberOfActualVersionedScripts ::
  forall r.
  SidechainParams ->
  -- | Number of expected versionned minting policy scripts
  Int ->
  -- | Number of expected versionned validator scripts
  Int ->
  Run
    (EXCEPT OffchainError + TRANSACTION + WALLET + READER Env + AFF + EFFECT + r)
    Unit
assertNumberOfActualVersionedScripts
  sidechainParams
  numExpectedVersionedPolicies
  numExpectedVersionedValidators = do
  Versioning.getActualVersionedPoliciesAndValidators sidechainParams
    >>=
      \{ versionedPolicies, versionedValidators } -> do
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
