module Test.InitSidechain.FUEL
  ( tests
  ) where

import Contract.Prelude

import Contract.AssocMap as Plutus.Map
import Contract.PlutusData (toData)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.List as List
import Mote.Monad as Mote.Monad
import Run (liftEffect) as Run
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
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.FUEL as InitFuel
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto as Crypto
import TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , getExpectedVersionedPoliciesAndValidators
  ) as Versioning
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( MerkleRootTokenPolicy
      , MerkleRootTokenValidator
      , DsKeyPolicy
      , FUELMintingPolicy
      , FUELBurningPolicy
      , CommitteeHashValidator
      , CommitteeCandidateValidator
      , CommitteeCertificateVerificationPolicy
      , CommitteeOraclePolicy
      )
  )

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Initialising FUEL" $ do
  -- InitFuel endpoint
  initFuelSucceeds
  initFuelIdempotent

-- | A call to `initTokensMint` is followed by `initFuel`, resulting in success.
initFuelSucceeds ∷ PlutipTest
initFuelSucceeds =
  Mote.Monad.test "`initFuel` succeeds"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initFuelSucceeds'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let committeeSize = 25
          committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
            committeeSize
            Crypto.generatePrivKey

          let
            version = 1
            initATMSKind = ATMSPlainEcdsaSecp256k1
            initSidechainEpoch = zero
            initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
            initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
              unwrap
              initCommittee
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }

          -- First create init tokens
          void $ InitMint.initTokensMint sidechainParams initATMSKind version

          { scriptsInitTxIds, tokensInitTxId } ←
            InitFuel.initFuel sidechainParams
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              version

          -- Which tokens are on the wallet?  Were the DS init tokens burned?
          { initTokenStatusData: tokenStatus } ← Init.getInitTokenStatus
            sidechainParams

          -- Which scripts are actually being versioned?
          { versionedPolicies: actualPolicies
          , versionedValidators: actualValidators
          } ←
            Versioning.getActualVersionedPoliciesAndValidators
              { atmsKind: initATMSKind
              , sidechainParams
              }
              version

          -- For computing the number of versionOracle init tokens
          { versionedPolicies, versionedValidators } ←
            Versioning.getExpectedVersionedPoliciesAndValidators
              { atmsKind: initATMSKind
              , sidechainParams
              }
              version

          let
            dsSpent = not $
              Plutus.Map.member
                DistributedSet.dsInitTokenName
                tokenStatus
            expectedTokens = expectedInitTokens
              ( List.length expectedExistingValidators +
                  List.length expectedExistingPolicies
              )
              versionedPolicies
              versionedValidators
              [ Checkpoint.checkpointInitTokenName
              , CandidatePermissionToken.candidatePermissionInitTokenName
              ]
            expectedExistingValidators = Array.toUnfoldable
              [ CommitteeHashValidator
              , CommitteeCandidateValidator
              , MerkleRootTokenValidator
              ]
            expectedExistingPolicies = Array.toUnfoldable
              [ CommitteeCertificateVerificationPolicy
              , CommitteeOraclePolicy
              , DsKeyPolicy
              , FUELMintingPolicy
              , FUELBurningPolicy
              , MerkleRootTokenPolicy
              ]
            actualExistingValidators = map fst actualValidators
            actualExistingPolicies = map fst actualPolicies

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert "FUEL init not finalized"
                (length scriptsInitTxIds == 9 && not (isNothing tokensInitTxId))
            <* assert "DS init token not spent" dsSpent
            <* assert
              ( "Incorrect tokens.  " <>
                  failMsg expectedTokens tokenStatus
              )
              (unorderedEq expectedTokens tokenStatus)
            <* assert
              ( "Incorrect validators.  " <>
                  failMsg expectedExistingValidators actualExistingValidators
              )
              ( List.sort expectedExistingValidators ==
                  List.sort actualExistingValidators
              )
            <* assert
              ( "Incorrect policies.  " <>
                  failMsg expectedExistingPolicies actualExistingPolicies
              )
              ( List.sort expectedExistingPolicies ==
                  List.sort actualExistingPolicies
              )

-- | Second call to `initFuel` should return `Nothing`.
initFuelIdempotent ∷ PlutipTest
initFuelIdempotent =
  Mote.Monad.test "`initFuel` called a second time returns Nothing"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initFuelIdempotent'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let committeeSize = 25
          committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
            committeeSize
            Crypto.generatePrivKey

          let
            version = 1
            initSidechainEpoch = zero
            initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
            initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
              unwrap
              initCommittee
            initATMSKind = ATMSPlainEcdsaSecp256k1
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }

          -- First create init tokens
          void $ InitMint.initTokensMint sidechainParams initATMSKind version

          void $ InitFuel.initFuel sidechainParams
            initSidechainEpoch
            initAggregatedCommittee
            initATMSKind
            version

          -- Second call should do nothing.
          { scriptsInitTxIds, tokensInitTxId } ←
            InitFuel.initFuel sidechainParams
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              version

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert "Second call to initFuel submitted at least one transaction"
                (null scriptsInitTxIds && isNothing tokensInitTxId)
