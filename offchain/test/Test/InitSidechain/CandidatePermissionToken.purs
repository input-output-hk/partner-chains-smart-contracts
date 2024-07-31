module Test.InitSidechain.CandidatePermissionToken where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Log as Log
import Contract.Wallet as Wallet
import JS.BigInt (fromInt)
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.InitSidechain.Utils (failMsg)
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Unit.Assert (assert)
import Test.Utils (WrappedTests, testnetGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.CandidatePermissionToken as InitCandidatePermission
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = testnetGroup "Initialising the candidate permission token mechanism" $
  do
    -- InitCandidatePermissionToken endpoint
    testInitCandidatePermissionToken
    testInitCandidatePermissionTokenIdempotent

-- | Test `initCandidatePermissionToken` having run `initTokensMint`, expecting
-- | no failure
-- Note that this test isn't great. If we want to keep the
-- `initCandidatePermissionToken` machinery, we should improve this test.
testInitCandidatePermissionToken ∷ TestnetTest
testInitCandidatePermissionToken =
  Mote.Monad.test "Calling `InitCandidatePermissionToken`"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCandidatePermissionToken'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            initGovernanceAuthority ← Governance.mkGovernanceAuthority
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initCandidatePermissionTokenMintInfo = Just (fromInt 1)
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

            void $ InitCandidatePermission.initCandidatePermissionToken
              sidechainParams
              initCandidatePermissionTokenMintInfo

-- | Test running `initCandidatePermissionToken` twice, having run
-- | `initTokensMint`, expecting idempotency
testInitCandidatePermissionTokenIdempotent ∷ TestnetTest
testInitCandidatePermissionTokenIdempotent =
  Mote.Monad.test
    "Calling `InitCandidatePermissionToken` twice, expecting idempotency"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCandidatePermissionTokenIdempotent'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            initGovernanceAuthority ← Governance.mkGovernanceAuthority
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initCandidatePermissionTokenMintInfo = Just (fromInt 1)
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            -- Initialise tokens
            void $ InitMint.initTokensMint sidechainParams
              initATMSKind
              version

            -- Initialise checkpoint
            void $ InitCandidatePermission.initCandidatePermissionToken
              sidechainParams
              initCandidatePermissionTokenMintInfo

            -- Then do it again
            res ← InitCandidatePermission.initCandidatePermissionToken
              sidechainParams
              initCandidatePermissionTokenMintInfo

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $ assert (failMsg "Nothing" res) (isNothing res)
