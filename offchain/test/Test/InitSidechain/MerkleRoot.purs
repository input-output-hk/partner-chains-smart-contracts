module Test.InitSidechain.MerkleRoot (tests) where

import Contract.Prelude

import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Data.List as List
import Mote.Monad as Mote.Monad
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
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.MerkleRoot as InitMerkleRoot
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , getExpectedVersionedPoliciesAndValidators
  ) as Versioning
import TrustlessSidechain.Versioning.Types
  ( ScriptId(MerkleRootTokenPolicy, MerkleRootTokenValidator)
  )

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Initialising MerkleRoot" $ do
  -- InitMerkleRoot endpoint
  initMerkleRootSucceeds
  initMerkleRootIdempotent

-- | A call to `initTokensMint` is followed by `MerkleRoot`, resulting in
-- | success.
initMerkleRootSucceeds ∷ PlutipTest
initMerkleRootSucceeds =
  Mote.Monad.test "`initMerkleRoot` succeeds"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initMerkleRootSucceeds'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            version = 1
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

          merkleRootRes ← InitMerkleRoot.initMerkleRoot sidechainParams
            initATMSKind
            version

          { initTokenStatusData: tokenStatus } ← Init.getInitTokenStatus
            sidechainParams

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
            expected = true
            res = isJust merkleRootRes
            expectedTokens = expectedInitTokens 2 versionedPolicies
              versionedValidators
              [ Checkpoint.checkpointInitTokenName
              , DistributedSet.dsInitTokenName
              , CommitteeOraclePolicy.committeeOracleInitTokenName
              , CandidatePermissionToken.candidatePermissionInitTokenName
              ]
            expectedScripts = List.fromFoldable
              [ MerkleRootTokenPolicy, MerkleRootTokenValidator ]
            actualScripts = (map fst actualPolicies) <> (map fst actualValidators)

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) res
            <* assert (failMsg expectedTokens tokenStatus)
              (unorderedEq expectedTokens tokenStatus)
            <* assert (failMsg expectedScripts actualScripts)
              (expectedScripts == actualScripts)

-- | Second call to `initMerkleRoot` should return `Nothing`.
initMerkleRootIdempotent ∷ PlutipTest
initMerkleRootIdempotent =
  Mote.Monad.test "`initMerkleRoot` called a second time returns Nothing"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initMerkleRootIdempotent'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            version = 1
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

          void $ InitMerkleRoot.initMerkleRoot sidechainParams initATMSKind
            version

          -- Second call should do nothing.
          merkleRootRes ← InitMerkleRoot.initMerkleRoot sidechainParams
            initATMSKind
            version

          let
            expected = true
            res = isNothing merkleRootRes

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) res
