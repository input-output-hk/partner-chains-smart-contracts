module Test.InitSidechain.TokensMint
  ( tests
  ) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Wallet as Wallet
import JS.BigInt as BigInt
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
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Versioning as Versioning

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Minting the init tokens" $ do
  -- InitTokensMint endpoint
  initTokensMintScenario1
  initTokensMintIdempotent

-- | Setup the same as `testScenario1`, but in which `initTokensMint`
-- | is called rather than `initSidechain`. It should succeed, and
-- | it checks the tokens are indeed minted by calling `getInitTokenStatus`.
initTokensMintScenario1 ∷ PlutipTest
initTokensMintScenario1 =
  Mote.Monad.test "`initTokensMint` returns expected token names and quantities"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initTokensMintScenario1'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            Governance.mkGovernanceAuthority
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

          -- Command being tested
          void $ InitMint.initTokensMint sidechainParams initATMSKind version

          -- For computing the number of versionOracle init tokens
          { versionedPolicies, versionedValidators } ←
            Versioning.getExpectedVersionedPoliciesAndValidators
              { atmsKind: initATMSKind
              , sidechainParams
              }
              version

          let
            expected = expectedInitTokens 0 versionedPolicies versionedValidators
              [ Checkpoint.checkpointInitTokenName
              , DistributedSet.dsInitTokenName
              , CommitteeOraclePolicy.committeeOracleInitTokenName
              , CandidatePermissionToken.candidatePermissionInitTokenName
              ]

          -- Get the tokens just created
          { initTokenStatusData: res } ← Init.getInitTokenStatus
            sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res)
                (unorderedEq expected res)

-- | Same as `initTokensMintScenario1`, except this
-- | attempts to mint the tokens twice. The tokens should
-- | still be as expected and the return transactionId should
-- | be `Nothing`.
initTokensMintIdempotent ∷ PlutipTest
initTokensMintIdempotent =
  Mote.Monad.test "`initTokensMint` gives expected results when called twice"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initTokensMintIdempotent'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            Governance.mkGovernanceAuthority
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

          -- Mint them once
          void $ InitMint.initTokensMint sidechainParams
            initATMSKind
            version

          -- Then do it again.
          { transactionId } ← InitMint.initTokensMint sidechainParams
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
            expected = expectedInitTokens 0 versionedPolicies versionedValidators
              [ Checkpoint.checkpointInitTokenName
              , DistributedSet.dsInitTokenName
              , CommitteeOraclePolicy.committeeOracleInitTokenName
              , CandidatePermissionToken.candidatePermissionInitTokenName
              ]

          -- Get the tokens just created
          { initTokenStatusData: res } ← Init.getInitTokenStatus
            sidechainParams

          -- Resulting tokens are as expected
          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res)
                (unorderedEq expected res)

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg "Nothing" transactionId) (isNothing transactionId)
