module Test.InitSidechain.TokensMint
  ( suite
  ) where

import Contract.Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Test.Testnet (withWallets)
import Contract.Wallet (withKeyWallet)
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Test.InitSidechain.Utils (expectedInitTokens, failMsg, unorderedEq)
import Test.Unit.Assert (assert)
import Test.Utils (TestnetTest)
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.Env (emptyEnv)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (unliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Versioning as Versioning

suite :: TestnetTest
suite = group "Minting the init tokens" $ do
  -- InitTokensMint endpoint
  initTokensMintIdempotent

-- | This test calls `initTokensMint` twice to check its idempotency.
-- | It should succeed on both calls, with the following expectations:
-- | 1. The tokens are minted correctly (verified by calling `getInitTokenStatus`).
-- | 2. On the second call, the return transactionId should be `Nothing`.
-- | 3. The minted tokens should match the expected values after both calls.
initTokensMintIdempotent :: TestnetTest
initTokensMintIdempotent =
  test "`initTokensMint` gives expected results when called twice" do
    let
      initialDistribution =
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do
        Effect.logInfo' "InitSidechain 'initTokensMintIdempotent'"

        genesisUtxo <- Test.Utils.getOwnTransactionInput

        initGovernanceAuthority <-
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
        { transactionId } <- InitMint.initTokensMint sidechainParams
          version

        -- For computing the number of versionOracle init tokens
        { versionedPolicies, versionedValidators } <-
          Versioning.getExpectedVersionedPoliciesAndValidators
            sidechainParams
            version

        let
          expected = expectedInitTokens 0 versionedPolicies versionedValidators
            []

        -- Get the tokens just created
        { initTokenStatusData: res } <- Init.getInitTokenStatus
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
