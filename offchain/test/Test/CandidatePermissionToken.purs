module Test.CandidatePermissionToken
  ( tests
  , testScenarioFailure1
  , testScenarioSuccess1
  , assertIHaveCandidatePermissionToken
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Scripts as Scripts
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.CommitteeCandidateValidator as Test.CommitteeCandidateValidator
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests)
import Test.Utils as Test.Utils
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  , CandidatePermissionMintParams(CandidatePermissionMintParams)
  , CandidatePermissionTokenInfo
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.SidechainParams (SidechainParams)

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
    $ \alice → Wallet.withKeyWallet alice do

        myTxInput ← Test.Utils.getOwnTransactionInput
        let
          scParams = Test.Utils.dummySidechainParams
          candidateMintPermissionMint =
            CandidatePermissionMint
              { sidechainParams: scParams
              , candidatePermissionTokenUtxo: myTxInput
              }
          -- doesn't matter, so just use ada's token name
          candidatePermissionTokenName = Value.adaToken

        -----------------------------
        -- Running the candidate permission token endpoint..
        -----------------------------
        _ ← CandidatePermissionToken.runCandidatePermissionToken $
          CandidatePermissionMintParams
            { candidateMintPermissionMint
            , candidatePermissionTokenName: candidatePermissionTokenName
            , amount: BigInt.fromInt 1
            }

        -----------------------------
        -- Running the register endpoint
        -----------------------------
        registerTxId ←
          Test.CommitteeCandidateValidator.runRegisterWithCandidatePermissionInfo
            ( Just
                { candidatePermissionTokenUtxo: myTxInput
                , candidatePermissionTokenName
                }
            )
            scParams

        -----------------------------
        -- Asserting that the committee validator actually has the token
        -----------------------------
        { candidatePermissionCurrencySymbol } ←
          CandidatePermissionToken.getCandidatePermissionMintingPolicy
            candidateMintPermissionMint

        committeeCandidiateValidatorAddr ← do
          committeeCandidateValidator ←
            CommitteeCandidateValidator.getCommitteeCandidateValidator
              scParams
          netId ← Address.getNetworkId
          addr ← Monad.liftContractM ("Cannot get validator address") $
            Address.validatorHashEnterpriseAddress
              netId
              (Scripts.validatorHash committeeCandidateValidator)
          pure addr

        Test.Utils.assertHasOutputWithAsset registerTxId
          committeeCandidiateValidatorAddr
          candidatePermissionCurrencySymbol
          candidatePermissionTokenName

        -----------------------------
        -- Running the deregister endpoint
        -----------------------------
        _ ← Test.CommitteeCandidateValidator.runDeregister
          scParams

        Test.Utils.assertIHaveOutputWithAsset candidatePermissionCurrencySymbol
          candidatePermissionTokenName

        pure unit

-- | `assertIHaveCandidatePermissionToken` asserts that we have a UTxO with at
-- | least one of the candidate candidate permission token
assertIHaveCandidatePermissionToken ∷
  SidechainParams → CandidatePermissionTokenInfo → Contract Unit
assertIHaveCandidatePermissionToken
  scParams
  { candidatePermissionTokenUtxo, candidatePermissionTokenName } = do
  let
    candidateMintPermissionMint =
      CandidatePermissionMint
        { sidechainParams: scParams
        , candidatePermissionTokenUtxo
        }

  { candidatePermissionCurrencySymbol } ←
    CandidatePermissionToken.getCandidatePermissionMintingPolicy
      candidateMintPermissionMint

  Test.Utils.assertIHaveOutputWithAsset candidatePermissionCurrencySymbol
    candidatePermissionTokenName

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
    $ \alice → Wallet.withKeyWallet alice do

        myTxInput ← Test.Utils.getOwnTransactionInput
        let
          scParams = Test.Utils.dummySidechainParams
          candidateMintPermissionMint =
            CandidatePermissionMint
              { sidechainParams: scParams
              , candidatePermissionTokenUtxo: myTxInput
              }
          -- doesn't matter, so just use ada's token name
          tokenName = Value.adaToken

        -----------------------------
        -- Running the endpoints..
        -----------------------------
        txId ←
          Test.CommitteeCandidateValidator.runRegisterWithCandidatePermissionInfo
            Nothing
            scParams

        -----------------------------
        -- Asserting that the committee validator actually has the token
        -----------------------------
        { candidatePermissionCurrencySymbol } ←
          CandidatePermissionToken.getCandidatePermissionMintingPolicy
            candidateMintPermissionMint

        committeeCandidiateValidatorAddr ← do
          committeeCandidateValidator ←
            CommitteeCandidateValidator.getCommitteeCandidateValidator
              scParams
          netId ← Address.getNetworkId
          addr ← Monad.liftContractM ("Cannot get validator address") $
            Address.validatorHashEnterpriseAddress
              netId
              (Scripts.validatorHash committeeCandidateValidator)
          pure addr

        Test.Utils.assertHasOutputWithAsset txId committeeCandidiateValidatorAddr
          candidatePermissionCurrencySymbol
          tokenName
          # Test.Utils.fails

        pure unit
