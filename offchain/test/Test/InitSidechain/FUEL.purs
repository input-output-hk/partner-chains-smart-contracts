module Test.InitSidechain.FUEL
  ( tests
  ) where

import Contract.Prelude

import Contract.AssocMap as Plutus.Map
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
import TrustlessSidechain.InitSidechain.FUEL as InitFuel
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
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

          fuelRes ← InitFuel.initFuel sidechainParams initATMSKind version

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
            expectedTokens = expectedInitTokens (List.length expectedScripts)
              versionedPolicies
              versionedValidators
              [ Checkpoint.checkpointInitTokenName
              , CommitteeOraclePolicy.committeeOracleInitTokenName
              , CandidatePermissionToken.candidatePermissionInitTokenName
              ]
            expectedScripts = List.fromFoldable
              [ DsKeyPolicy
              , FUELMintingPolicy
              , FUELBurningPolicy
              , MerkleRootTokenPolicy
              , MerkleRootTokenValidator
              ]
            actualScripts = (map fst actualPolicies) <> (map fst actualValidators)

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert "FUEL init not attempted" (isJust fuelRes)
            <* assert "DS init token not spent" dsSpent
            <* assert (failMsg expectedTokens tokenStatus)
              (unorderedEq expectedTokens tokenStatus)
            <* assert (failMsg expectedScripts actualScripts)
              (List.sort expectedScripts == List.sort actualScripts)

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

          void $ InitFuel.initFuel sidechainParams initATMSKind version

          -- Second call should do nothing.
          fuelRes ← InitFuel.initFuel sidechainParams initATMSKind version

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert "Second call to initFuel submitted at least one transaction"
                (isNothing fuelRes)
