module Test.CandidatePermissionToken
  ( tests
  , testScenarioFailure1
  , testScenarioSuccess1
  , assertIHaveCandidatePermissionToken
  ) where

import Contract.Prelude

import Contract.Scripts as Scripts
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run (Run)
import Run.Except (EXCEPT)
import Test.CommitteeCandidateValidator as Test.CommitteeCandidateValidator
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests)
import Test.Utils as Test.Utils
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.Effects.Contract (CONTRACT)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain (initSpendGenesisUtxo)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address as Utils
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import Type.Row (type (+))

-- | `tests` wraps up all the tests conveniently
tests ∷ WrappedTests
tests =
  Test.Utils.plutipGroup "Candidate permission token register / deregister tests"
    $ do
        testScenarioSuccess1
        testScenarioFailure1

-- | Mint a single permission token, then register and verify that the
-- | permission token is at the committee validator address.... Then we
-- | deregister and if we get the permission token back.
testScenarioSuccess1 ∷ PlutipTest
testScenarioSuccess1 =
  Mote.Monad.test
    "Mint permission token, register (check if register validator has permission token), deregister (check if we get permission token back)"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        -- Generate genesis UTxO
        genesisUtxo ← Test.Utils.getOwnTransactionInput

        let
          -- Put genesis UTxO into sidechain params
          sidechainParams = wrap
            ( (unwrap Test.Utils.dummySidechainParams)
                { genesisUtxo = genesisUtxo }
            )

        -----------------------------
        -- Mint candidate permission init token and use it to mint a single
        -- candidate permission token
        -----------------------------
        _ ←
          ( (<>) <$> initSpendGenesisUtxo sidechainParams <*>
              CandidatePermissionToken.mintOneCandidatePermissionInitToken
                sidechainParams
          ) >>=
            balanceSignAndSubmit "Mint candidate permission init token"

        _ ← CandidatePermissionToken.runCandidatePermissionToken
          sidechainParams
          (BigInt.fromInt 1)
        -----------------------------
        -- Register candidate using a permission token
        -----------------------------
        registerTxId ←
          Test.CommitteeCandidateValidator.runRegisterWithCandidatePermissionInfo
            true
            sidechainParams

        -----------------------------
        -- Asserting that the committee validator actually has the token
        -----------------------------
        candidatePermissionInfo ←
          CandidatePermissionToken.candidatePermissionCurrencyInfo
            sidechainParams

        committeeCandidiateValidatorAddr ← do
          committeeCandidateValidator ←
            CommitteeCandidateValidator.getCommitteeCandidateValidator
              sidechainParams
          Utils.toAddress (Scripts.validatorHash committeeCandidateValidator)

        Test.Utils.assertHasOutputWithAsset registerTxId
          committeeCandidiateValidatorAddr
          candidatePermissionInfo.currencySymbol
          CandidatePermissionToken.candidatePermissionTokenName

        -----------------------------
        -- Running the deregister endpoint
        -----------------------------
        _ ← Test.CommitteeCandidateValidator.runDeregister sidechainParams

        Test.Utils.assertIHaveOutputWithAsset
          candidatePermissionInfo.currencySymbol
          CandidatePermissionToken.candidatePermissionTokenName

        pure unit

-- | `assertIHaveCandidatePermissionToken` asserts that we have a UTxO with at
-- | least one of the candidate candidate permission token
assertIHaveCandidatePermissionToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + CONTRACT + r) Unit
assertIHaveCandidatePermissionToken sidechainParams = do
  candidatePermissionInfo ←
    CandidatePermissionToken.candidatePermissionCurrencyInfo
      sidechainParams

  Test.Utils.assertIHaveOutputWithAsset
    candidatePermissionInfo.currencySymbol
    CandidatePermissionToken.candidatePermissionTokenName

  pure unit

-- | Check if there are no permission tokens paid to the register validator,
-- | then there are no tokens in the regsiter validator (this is essentialy
-- | tautology)
testScenarioFailure1 ∷ PlutipTest
testScenarioFailure1 =
  Mote.Monad.test
    "Register, check if register output doesn't have permission token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        , BigInt.fromInt 5_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do

        let
          sidechainParams = Test.Utils.dummySidechainParams

        -----------------------------
        -- Running the endpoints..
        -----------------------------
        txId ←
          Test.CommitteeCandidateValidator.runRegisterWithCandidatePermissionInfo
            false
            sidechainParams

        -----------------------------
        -- Asserting that the committee validator actually has the token
        -----------------------------
        candidatePermissionInfo ←
          CandidatePermissionToken.candidatePermissionCurrencyInfo
            sidechainParams

        committeeCandidiateValidatorAddr ← do
          committeeCandidateValidator ←
            CommitteeCandidateValidator.getCommitteeCandidateValidator
              sidechainParams
          Utils.toAddress (Scripts.validatorHash committeeCandidateValidator)

        Test.Utils.assertHasOutputWithAsset txId committeeCandidiateValidatorAddr
          candidatePermissionInfo.currencySymbol
          CandidatePermissionToken.candidatePermissionTokenName
          # withUnliftApp Test.Utils.fails

        pure unit
