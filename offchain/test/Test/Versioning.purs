module Test.Versioning (tests) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.List as List
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert (assert)
import Test.Utils
  ( WrappedTests
  , fails
  , getOwnTransactionInput
  , plutipGroup
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.FUELMintingPolicy.V2 as FUELMintingPolicy.V2
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  , toSidechainParams
  )
import TrustlessSidechain.MerkleRoot
  ( merkleRootCurrencyInfo
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning (getActualVersionedPoliciesAndValidators) as Versioning
import TrustlessSidechain.Versioning.Types (ScriptId(..))
import TrustlessSidechain.Versioning.Utils
  ( insertVersionLookupsAndConstraints
  , invalidateVersionLookupsAndConstraints
  ) as Versioning

-- | `tests` aggregate all the Versioning tests in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Minting and burning versioning tokens" $ do
  testInsertAndInvalidateSuccessScenario
  testInsertSameScriptTwiceSuccessScenario
  testInsertUnversionedScriptSuccessScenario
  testRemovingTwiceSameScriptFailScenario
  testRemovingScriptInsertedMultipleTimesSuccessScenario

-- | We insert a new version of a script, and invalidate the old one.
testInsertAndInvalidateSuccessScenario ∷ PlutipTest
testInsertAndInvalidateSuccessScenario =
  Mote.Monad.test "Inserting new version, then invalidate the old one"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initATMSKind = ATMSPlainEcdsaSecp256k1
          (initScParams@(InitSidechainParams initTokenParams)) =
            InitSidechainParams
              { initChainId: BigInt.fromInt 1
              , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ aggregateKeys
                  $ map unwrap initCommitteePubKeys
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                  pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams: toSidechainParams initTokenParams
            , atmsKind: initATMSKind
            }

        -- No versioned scripts are inserted.
        -- Assert that the actual versioned scripts set is empty
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 0 0
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- Insert all initial versioned scripts
        void $ initSidechain initScParams 1
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- We assume that the FUELMintingPolicy was already inserted in the `initSidechain` call.
        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              FUELMintingPolicy
          >>= balanceSignAndSubmit "Test: invalidate policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        fuelMintingPolicyV2 ← FUELMintingPolicy.V2.getFuelMintingPolicy
          sidechainParams
        void
          $ Versioning.insertVersionLookupsAndConstraints
              sidechainParams
              2
              (FUELMintingPolicy /\ fuelMintingPolicyV2.fuelMintingPolicy)
          >>=
            balanceSignAndSubmit "Test: insert new policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 1 0

-- | We insert the same script (same ScriptId and same version) twice. That should work.
testInsertSameScriptTwiceSuccessScenario ∷ PlutipTest
testInsertSameScriptTwiceSuccessScenario =
  Mote.Monad.test "Insert same script with the same version twice"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initATMSKind = ATMSPlainEcdsaSecp256k1
          (initScParams@(InitSidechainParams initTokenParams)) =
            InitSidechainParams
              { initChainId: BigInt.fromInt 1
              , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ aggregateKeys
                  $ map unwrap initCommitteePubKeys
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                  pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams: toSidechainParams initTokenParams
            , atmsKind: initATMSKind
            }

        void $ initSidechain initScParams 1
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        { mintingPolicy: merkleRootTokenMintingPolicy } ←
          merkleRootCurrencyInfo sidechainParams

        -- This policy was already inserted by 'initSidechain'.
        void
          $ Versioning.insertVersionLookupsAndConstraints
              sidechainParams
              1
              (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
          >>=
            balanceSignAndSubmit "Test: insert already versioned policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 7 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              MerkleRootTokenPolicy
          >>= balanceSignAndSubmit "Test: invalidate policy version 1"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              MerkleRootTokenPolicy
          >>= balanceSignAndSubmit "Test: invalidate policy version 2"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

-- | We insert an script that is not part of the initial versioned scripts.
testInsertUnversionedScriptSuccessScenario ∷ PlutipTest
testInsertUnversionedScriptSuccessScenario =
  Mote.Monad.test "Insert an unversioned script"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initATMSKind = ATMSPlainEcdsaSecp256k1
          (initScParams@(InitSidechainParams initTokenParams)) =
            InitSidechainParams
              { initChainId: BigInt.fromInt 1
              , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ aggregateKeys
                  $ map unwrap initCommitteePubKeys
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                  pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams
            , atmsKind: initATMSKind
            }

        void $ initSidechain initScParams 1
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        committeeCandidateValidator ← getCommitteeCandidateValidator
          sidechainParams

        -- This validator is not part of the versioned scripts hardcoded list, so it should *not* be
        -- inserted.
        void
          $ Versioning.insertVersionLookupsAndConstraints
              sidechainParams
              2
              (CommitteeCandidateValidator /\ committeeCandidateValidator)
          >>=
            balanceSignAndSubmit "Test: insert non-versioned validator version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 5
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

-- | After inserting a versioned script, invalidating it twice should fail in the second
-- | invalidation call.
testRemovingTwiceSameScriptFailScenario ∷ PlutipTest
testRemovingTwiceSameScriptFailScenario =
  Mote.Monad.test "Removing the same script twice should fail"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initATMSKind = ATMSPlainEcdsaSecp256k1
          (initScParams@(InitSidechainParams initTokenParams)) =
            InitSidechainParams
              { initChainId: BigInt.fromInt 1
              , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ aggregateKeys
                  $ map unwrap initCommitteePubKeys
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                  pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams
            , atmsKind: initATMSKind
            }

        void $ initSidechain initScParams 1
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- We assume this policy was already inserted by 'initSidechain', and thus try to invalidate
        -- it.
        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              MerkleRootTokenPolicy
          >>= balanceSignAndSubmit "Test: invalidate policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- We already invalidated that script. Re-invalidating it should fail.
        fails $ do
          void
            $ Versioning.invalidateVersionLookupsAndConstraints
                sidechainParams
                1
                MerkleRootTokenPolicy
            >>= balanceSignAndSubmit
              "Test: invalidate an already invalidated policy version"

-- | Executing a single versioned script invalidation transaction should remove a single versioned
-- | script, even if multiple scripts with the same ID and version were inserted.
testRemovingScriptInsertedMultipleTimesSuccessScenario ∷ PlutipTest
testRemovingScriptInsertedMultipleTimesSuccessScenario =
  Mote.Monad.test "Removing the same script twice should fail"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
        let
          initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
          initATMSKind = ATMSPlainEcdsaSecp256k1
          (initScParams@(InitSidechainParams initTokenParams)) =
            InitSidechainParams
              { initChainId: BigInt.fromInt 1
              , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ aggregateKeys
                  $ map unwrap initCommitteePubKeys
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                  pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams
            , atmsKind: initATMSKind
            }

        void $ initSidechain initScParams 1
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        { mintingPolicy: merkleRootTokenMintingPolicy } ←
          merkleRootCurrencyInfo sidechainParams

        -- This policy was already inserted by 'initSidechain'.
        void
          $ Versioning.insertVersionLookupsAndConstraints
              sidechainParams
              1
              (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
          >>=
            balanceSignAndSubmit "Test: insert already versioned policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 7 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- We assume this policy was already inserted by 'initSidechain', and thus try to invalidate
        -- it.
        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              MerkleRootTokenPolicy
          >>= balanceSignAndSubmit "Test: invalidate policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- Now, we invalidate the duplicated policy.
        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              MerkleRootTokenPolicy
          >>= balanceSignAndSubmit "Test: invalidate policy version"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

assertNumberOfActualVersionedScripts ∷
  { sidechainParams ∷ SidechainParams, atmsKind ∷ ATMSKinds } →
  -- | Version number
  Int →
  -- | Number of expected versionned minting policy scripts
  Int →
  -- | Number of expected versionned validator scripts
  Int →
  Contract Unit
assertNumberOfActualVersionedScripts
  sidechainParamsWithATMSKind
  version
  numExpectedVersionedPolicies
  numExpectedVersionedValidators = do
  Versioning.getActualVersionedPoliciesAndValidators sidechainParamsWithATMSKind
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
