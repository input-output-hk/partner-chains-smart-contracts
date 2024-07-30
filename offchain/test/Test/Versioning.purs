module Test.Versioning (tests) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.List as List
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT)
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert (assert)
import Test.Utils
  ( WrappedTests
  , fails
  , getOwnTransactionInput
  , plutipGroup
  , withSingleMultiSig
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.FUELMintingPolicy.V2 as FUELMintingPolicy.V2
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  , toSidechainParams
  )
import TrustlessSidechain.MerkleRoot (merkleRootCurrencyInfo)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , insertVersion
  ) as Versioning
import TrustlessSidechain.Versioning.Types (ScriptId(..))
import TrustlessSidechain.Versioning.Utils
  ( insertVersionLookupsAndConstraints
  , invalidateVersionLookupsAndConstraints
  ) as Versioning
import Type.Row (type (+))

-- | `tests` aggregate all the Versioning tests in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Minting and burning versioning tokens" $ do
  testInsertAndInvalidateSuccessScenario
  testInsertSameScriptTwiceSuccessScenario
  testInsertUnversionedScriptSuccessScenario
  testRemovingTwiceSameScriptFailScenario
  testRemovingScriptInsertedMultipleTimesSuccessScenario
  testInsertScriptsPresentInPreviousVersion

-- | We insert a new version of a script, and invalidate the old one.
testInsertAndInvalidateSuccessScenario ∷ PlutipTest
testInsertAndInvalidateSuccessScenario =
  Mote.Monad.test "Inserting new version, then invalidate the old one"
    $ Test.PlutipTest.mkPlutipConfigTest
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
            keyCount = 25
          initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
            generatePrivKey
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
                , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
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

          -- At this point we expect the following 8 policies to be inserted
          -- into the versioning system by the call to `initSidechain`:
          --
          --   * CommitteeCertificateVerificationPolicy
          --   * CommitteeOraclePolicy
          --   * ReserveAuthPolicy
          --   * GovernancePolicy
          --   * FUELMintingPolicy
          --   * FUELBurningPolicy
          --   * DsKeyPolicy
          --   * MerkleRootTokenPolicy
          --
          -- We also expect the following validators (6 in total):
          --
          --   * CommitteeHashValidator
          --   * CommitteeCandidateValidator
          --   * CheckpointValidator
          --   * ReserveValidator
          --   * IlliquidCirculationSupplyValidator
          --   * MerkleRootTokenValidator
          --
          -- Note that we have defined a multisig governance for this test,
          -- which means that ReserveAuthPolicy and GovernancePolicy policies,
          -- as well as ReserveValidator and IlliquidCirculationSupplyValidator
          -- get inserted.
          assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 8 6
          assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

          -- We now invalidate FUELMintingPolicy, which should result in one
          -- less policy present in the versioning system, for a total of 7.
          void
            $ Versioning.invalidateVersionLookupsAndConstraints
                sidechainParams
                1
                FUELMintingPolicy
            >>= balanceSignAndSubmit "Test: invalidate policy version"
          assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 7 6
          assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

          -- Now we insert a policy in version 2.  This should leave the count
          -- of version 1 scripts unaffected.
          fuelMintingPolicyV2 ← FUELMintingPolicy.V2.getFuelMintingPolicy
            sidechainParams
          void
            $ Versioning.insertVersionLookupsAndConstraints
                sidechainParams
                2
                (FUELMintingPolicy /\ fuelMintingPolicyV2.fuelMintingPolicy)
            >>=
              balanceSignAndSubmit "Test: insert new policy version"
          assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 7 6
          assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 1 0

-- | We insert the same script (same ScriptId and same version) twice. That
-- should work.
testInsertSameScriptTwiceSuccessScenario ∷ PlutipTest
testInsertSameScriptTwiceSuccessScenario =
  Mote.Monad.test "Insert same script with the same version twice"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
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
              , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams: toSidechainParams initTokenParams
            , atmsKind: initATMSKind
            }

        void $ initSidechain initScParams 1

        -- In this test we have not defined any governance.  As a result call to
        -- `initSidechain` does not initialize the native token management
        -- system and as a result there are two policies less and two validators
        -- less being inserted than in testInsertAndInvalidateSuccessScenario
        -- test.
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        { mintingPolicy: merkleRootTokenMintingPolicy } ←
          merkleRootCurrencyInfo sidechainParams

        -- This policy was already inserted by 'initSidechain'.  However, by
        -- using these low-level functions of the versioning system we can
        -- insert the same script multiple times.
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
          >>= balanceSignAndSubmit "Test: invalidate policy version 1, once more"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

-- | We insert an script that is not part of the initial versioned scripts.
testInsertUnversionedScriptSuccessScenario ∷ PlutipTest
testInsertUnversionedScriptSuccessScenario =
  Mote.Monad.test "Insert an unversioned script"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
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
              , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
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

        -- This validator is not part of the versioned scripts hardcoded list,
        -- so it should *not* be inserted.
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
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
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
              , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
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
        withUnliftApp fails $ do
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
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
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
              , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
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

-- | `insertVersion` only inserts a script with `version` for elements with
-- | `ScriptId` present among policies / validators of `version - 1`.
testInsertScriptsPresentInPreviousVersion ∷ PlutipTest
testInsertScriptsPresentInPreviousVersion =
  Mote.Monad.test "Insert new version of a script only if present in version - 1"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 40_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) $ do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← liftEffect $ sequence $ Array.replicate keyCount
          generatePrivKey
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
              , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
              , initATMSKind
              }
          sidechainParams = toSidechainParams initTokenParams
          sidechainParamsWithATMSKind =
            { sidechainParams: toSidechainParams initTokenParams
            , atmsKind: initATMSKind
            }

        -- Insert all initial versioned scripts
        void $ initSidechain initScParams 1

        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 6 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- Invalidate FUELMintingPolicy. It should not get updated.
        void
          $ Versioning.invalidateVersionLookupsAndConstraints
              sidechainParams
              1
              FUELMintingPolicy
          >>= balanceSignAndSubmit
            "Test: invalidate fuel minting policy version 1"
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 0 0

        -- Update all possible scripts to V2.
        -- NOTE: V2 currently contains only FuelMintingPolicy and FUELBurningPolicy,
        -- so without the minting policy this should insert only one V2 script.
        void $ Versioning.insertVersion sidechainParamsWithATMSKind 2

        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 1 5 4
        assertNumberOfActualVersionedScripts sidechainParamsWithATMSKind 2 1 0

assertNumberOfActualVersionedScripts ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
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
