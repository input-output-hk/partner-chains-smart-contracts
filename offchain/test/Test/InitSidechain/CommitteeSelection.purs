module Test.InitSidechain.CommitteeSelection
  ( tests
  ) where

import Contract.Prelude

import Contract.Log as Log
import Contract.PlutusData (toData)
import Contract.Wallet as Wallet
import Control.Monad.Error.Class as MonadError
import Data.Array (toUnfoldable)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.List (List)
import Mote.Monad as Mote.Monad
import Run (liftEffect) as Run
import Run.Except (throw)
import Test.InitSidechain.Utils (expectedInitTokens, failMsg, unorderedEq)
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert (assert)
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint.Utils as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.CommitteeSelection as InitCommittee
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto as Crypto
import TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , getExpectedVersionedPoliciesAndValidators
  ) as Versioning
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Initialising the committee selection mechanism" $ do
  -- InitCommitteeSelection endpoint
  testInitCommitteeSelection
  testInitCommitteeSelectionUninitialised
  testInitCommitteeSelectionIdempotent

-- | Test `initCommitteeSelection` having run `initTokensMint`, expecting success and for the
-- | the relevant tokens to be spent
testInitCommitteeSelection ∷ PlutipTest
testInitCommitteeSelection =
  Mote.Monad.test "Calling `InitCommitteeSelection`"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCommitteeSelection'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            -- generate an initialize committee of `committeeSize` committee members
            let committeeSize = 25
            committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
              committeeSize
              Crypto.generatePrivKey

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
              initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
                unwrap
                initCommittee
              initCandidatePermissionTokenMintInfo = Nothing
              initSidechainEpoch = zero
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitMint.initTokensMint sidechainParams
              initATMSKind
              version
            void $ InitCommittee.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              version

            -- For computing the number of versionOracle init tokens
            { versionedPolicies, versionedValidators } ←
              Versioning.getExpectedVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedTokens = expectedInitTokens 4 versionedPolicies
                versionedValidators
                [ DistributedSet.dsInitTokenName
                , Checkpoint.checkpointInitTokenName
                , CandidatePermissionToken.candidatePermissionInitTokenName
                ]

            -- Get the tokens just created
            { initTokenStatusData: resTokens } ← Init.getInitTokenStatus
              sidechainParams

            { versionedValidators: validatorsRes
            , versionedPolicies: policiesRes
            } ←
              Versioning.getActualVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedExistingValidators =
                [ CommitteeHashValidator
                , CommitteeCandidateValidator
                ]
              expectedExistingPolicies =
                [ CommitteeCertificateVerificationPolicy, CommitteeOraclePolicy ]
              actualExistingValidators = map fst validatorsRes
              actualExistingPolicies = map fst policiesRes

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $
                assert (failMsg expectedTokens resTokens)
                  (unorderedEq expectedTokens resTokens)
              <* assert
                ( failMsg expectedExistingValidators
                    actualExistingValidators
                )
                ( actualExistingValidators `sublist` toUnfoldable
                    expectedExistingValidators
                )
              <* assert
                ( failMsg expectedExistingPolicies
                    actualExistingPolicies
                )
                ( actualExistingPolicies `sublist` toUnfoldable
                    expectedExistingPolicies
                )

-- | Test `initCommitteeSelection` without having run `initTokensMint`, expecting failure
testInitCommitteeSelectionUninitialised ∷ PlutipTest
testInitCommitteeSelectionUninitialised =
  Mote.Monad.test "Calling `InitCommitteeSelection` with no init token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        result ← withUnliftApp (MonadError.try <<< Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCommitteeSelectionUninitialised'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            -- generate an initialize committee of `committeeSize` committee members
            let committeeSize = 25
            committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
              committeeSize
              Crypto.generatePrivKey

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
              initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
                unwrap
                initCommittee
              initCandidatePermissionTokenMintInfo = Nothing
              initSidechainEpoch = zero
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitCommittee.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              1
        case result of
          Right _ →
            throw $ GenericInternalError
              "Contract should have failed but it didn't."
          Left _err → pure unit

-- | Test running `initCommitteeSelection` twice, having run `initTokensMint`, expecting idempotency
-- | and for the relevant tokens to be spent
testInitCommitteeSelectionIdempotent ∷ PlutipTest
testInitCommitteeSelectionIdempotent =
  Mote.Monad.test "Calling `InitCommitteeSelection` twice, expecting idempotency"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCommitteeSelectionIdempotent'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            -- generate an initialize committee of `committeeSize` committee members
            let committeeSize = 25
            committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
              committeeSize
              Crypto.generatePrivKey

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
              initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
                unwrap
                initCommittee
              initCandidatePermissionTokenMintInfo = Nothing
              initSidechainEpoch = zero
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitMint.initTokensMint sidechainParams
              initATMSKind
              version
            void $ InitCommittee.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              version
            res ← InitCommittee.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              version

            -- For computing the number of versionOracle init tokens
            { versionedPolicies, versionedValidators } ←
              Versioning.getExpectedVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedTokens = expectedInitTokens 4 versionedPolicies
                versionedValidators
                [ DistributedSet.dsInitTokenName
                , Checkpoint.checkpointInitTokenName
                , CandidatePermissionToken.candidatePermissionInitTokenName
                ]

            -- Get the tokens just created
            { initTokenStatusData: resTokens } ← Init.getInitTokenStatus
              sidechainParams

            { versionedValidators: validatorsRes, versionedPolicies: policiesRes } ←
              Versioning.getActualVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedExistingValidators =
                [ CommitteeHashValidator
                , CommitteeCandidateValidator
                ]
              expectedExistingPolicies =
                [ CommitteeCertificateVerificationPolicy
                , CommitteeOraclePolicy
                ]
              actualExistingValidators = map fst validatorsRes
              actualExistingPolicies = map fst policiesRes

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $
                assert (failMsg expectedTokens resTokens)
                  (unorderedEq expectedTokens resTokens)
              <* assert (failMsg "Nothing" res) (isNothing res)
              <* assert
                ( failMsg expectedExistingValidators
                    actualExistingValidators
                )
                ( actualExistingValidators `sublist`
                    toUnfoldable expectedExistingValidators
                )
              <* assert
                ( failMsg expectedExistingPolicies
                    actualExistingPolicies
                )
                ( actualExistingPolicies `sublist` toUnfoldable
                    expectedExistingPolicies
                )

sublist ∷ ∀ a. Eq a ⇒ List a → List a → Boolean
sublist listA listB = all (\b → b `elem` listA) listB
