module Test.InitSidechain.TokensMint
  ( tests
  ) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Wallet as Wallet
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.InitSidechain.Utils (expectedInitTokens, failMsg, unorderedEq)
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Unit.Assert (assert)
import Test.Utils (WrappedTests, testnetGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
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
tests = testnetGroup "Minting the init tokens" $ do
  -- InitTokensMint endpoint
  initTokensMintIdempotent

-- | This test calls `initTokensMint` twice to check its idempotency.
-- | It should succeed on both calls, with the following expectations:
-- | 1. The tokens are minted correctly (verified by calling `getInitTokenStatus`).
-- | 2. On the second call, the return transactionId should be `Nothing`.
-- | 3. The minted tokens should match the expected values after both calls.
initTokensMintIdempotent ∷ TestnetTest
initTokensMintIdempotent =
  Mote.Monad.test "`initTokensMint` gives expected results when called twice"
    $ Test.TestnetTest.mkTestnetConfigTest
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
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }

          -- Mint them once
          void $ InitMint.initTokensMint sidechainParams
            version

          -- Then do it again.
          { transactionId } ← InitMint.initTokensMint sidechainParams
            version

          -- For computing the number of versionOracle init tokens
          { versionedPolicies, versionedValidators } ←
            Versioning.getExpectedVersionedPoliciesAndValidators
              sidechainParams
              version

          let
            expected = expectedInitTokens 0 versionedPolicies versionedValidators
              [ CandidatePermissionToken.candidatePermissionInitTokenName
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
