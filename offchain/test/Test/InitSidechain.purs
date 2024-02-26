module Test.InitSidechain
  ( tests
  ) where

import Contract.Prelude

import Contract.AssocMap as Plutus.Map
import Contract.Log as Log
import Contract.Monad as Monad
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Value as Value
import Contract.Wallet as Wallet
import Control.Monad.Error.Class as MonadError
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Set as Set
import Effect.Exception as Exception
import Mote.Monad as Mote.Monad
import Test.CandidatePermissionToken as Test.CandidatePermissionToken
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
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain as InitSidechain
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto as Crypto
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Initialising the sidechain" $ do
  -- InitSidechain endpoint
  testScenario1
  testScenario2
  testScenario3
  -- InitTokenStatus endpoint
  testInitTokenStatusEmpty
  testInitTokenStatusOneToken
  testInitTokenStatusMultiTokens

-- | `testScenario1` just calls the init sidechain endpoint (which should
-- | succeed!)
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "Calling `initSidechain`"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 50_000_000
      , BigInt.fromInt 50_000_000
      , BigInt.fromInt 50_000_000
      , BigInt.fromInt 50_000_000
      ]
  $ \alice →
      Wallet.withKeyWallet alice do
        Log.logInfo' "InitSidechain 'testScenario1'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        -- generate an initialize committee of `committeeSize` committee members
        let committeeSize = 25
        committeePrvKeys ← sequence $ Array.replicate committeeSize
          Crypto.generatePrivKey

        initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
          <$> getOwnPaymentPubKeyHash
        let
          initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
          initScParams = InitSidechain.InitSidechainParams
            { initChainId: BigInt.fromInt 69
            , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ Crypto.aggregateKeys $ map unwrap
                initCommittee
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initGovernanceAuthority
            }

        void $ InitSidechain.initSidechain initScParams 1

-- | `testScenario2` is a bit more complicated (but this should fail!) than
-- | `testScenario1`. It takes two distinct wallets, say Alice and Bob, grabs a
-- | utxo from Alice as the `initUtxo` (`genesisUtxo`); then Bob tries to
-- | initialize the sidechain with Alice's utxo.
-- | In short, this verifies that to initialize the sidechain, we need to spend
-- | the `initUtxo`
testScenario2 ∷ PlutipTest
testScenario2 = Mote.Monad.test "Verifying `initSidechain` spends `initUtxo`"
  $ Test.PlutipTest.mkPlutipConfigTest
      ( [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 10_000_000
        ] /\
          [ BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          ]
      )
  $ \(alice /\ bob) → do
      aliceUtxos ← Wallet.withKeyWallet alice $ Monad.liftedM
        "Failed to query wallet utxos"
        Wallet.getWalletUtxos

      genesisUtxo ← Monad.liftContractM "No utxo found in wallet"
        $ Set.findMin
        $ Map.keys aliceUtxos

      result ← MonadError.try $ Wallet.withKeyWallet bob do
        -- generate an initialize committee of `committeeSize` committee members
        let committeeSize = 1000
        committeePrvKeys ← sequence $ Array.replicate committeeSize
          Crypto.generatePrivKey
        initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
          <$> getOwnPaymentPubKeyHash
        let
          initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
          initScParams = InitSidechain.InitSidechainParams
            { initChainId: BigInt.fromInt 69
            , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ Crypto.aggregateKeys $ map unwrap
                initCommittee
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initGovernanceAuthority
            }

        void $ InitSidechain.initSidechain initScParams 1
      case result of
        Right _ →
          Monad.throwContractError $ Exception.error
            "Contract should have failed but it didn't."
        Left _err → pure unit

-- | `testScenario3` is identical to `testScenario1` BUT we include the minting
-- | of candidate permission tokens, and verify that we actually have the
-- | candidate permission token afterwards
testScenario3 ∷ PlutipTest
testScenario3 =
  Mote.Monad.test "Calling `initSidechain` with candidate permission tokens"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testScenario3'"
          genesisUtxo ← Test.Utils.getOwnTransactionInput
          -- generate an initialize committee of `committeeSize` committee members
          let committeeSize = 25
          committeePrvKeys ← sequence $ Array.replicate committeeSize
            Crypto.generatePrivKey
          initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
            <$> getOwnPaymentPubKeyHash
          let
            initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
            initScParams = InitSidechain.InitSidechainParams
              { initChainId: BigInt.fromInt 69
              , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ Crypto.aggregateKeys
                  $ map unwrap initCommittee
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initATMSKind: ATMSPlainEcdsaSecp256k1
              , initCandidatePermissionTokenMintInfo:
                  Just
                    { candidatePermissionTokenAmount: one
                    }
              , initGovernanceAuthority
              }

          { sidechainParams: sc } ← InitSidechain.initSidechain initScParams 1
          Test.CandidatePermissionToken.assertIHaveCandidatePermissionToken sc

-- | Utility for tests of getInitTokenStatus with a simple setup,
-- | taken from testScenario1. If the supplied candidate permission token amount
-- | is `Nothing`, the only init token remaining should be one with name
-- | "CandidatePermission InitToken", since in that case
-- | `initCandidatePermissionTokenLookupsAndConstraints` does not spend that
-- | token in `initSidechain`. Otherwise, no init tokens should remain.
initSimpleSidechain ∷
  Maybe { candidatePermissionTokenAmount ∷ BigInt.BigInt } →
  Monad.Contract SidechainParams.SidechainParams
initSimpleSidechain amt = do
  genesisUtxo ← Test.Utils.getOwnTransactionInput
  -- generate an initialize committee of `committeeSize` committee members
  let committeeSize = 25
  committeePrvKeys ← sequence $ Array.replicate committeeSize
    Crypto.generatePrivKey

  initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
    <$> getOwnPaymentPubKeyHash
  let
    initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
    initScParams = InitSidechain.InitSidechainParams
      { initChainId: BigInt.fromInt 69
      , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
      , initUtxo: genesisUtxo
      , initAggregatedCommittee: toData $ Crypto.aggregateKeys $ map unwrap
          initCommittee
      , initATMSKind: ATMSPlainEcdsaSecp256k1
      , initSidechainEpoch: zero
      , initThresholdNumerator: BigInt.fromInt 2
      , initThresholdDenominator: BigInt.fromInt 3
      , initCandidatePermissionTokenMintInfo: amt
      , initGovernanceAuthority
      }

  map _.sidechainParams $ InitSidechain.initSidechain initScParams 1

-- | Directly mint some init tokens with `mintOne*InitToken` actions,
-- | spending the genesis UTxO. Returns the names and quantities of tokens minted.
-- | Compare to `initSidechain`, where these tokens are created but are
-- | immediately spent by `init*LookupsAndConstraints` transactions.
-- | Note `balanceSignAndSubmit` blocks until the transactions are
-- | confirmed.
mintSeveralInitTokens ∷
  SidechainParams →
  Monad.Contract (Plutus.Map.Map Value.TokenName BigInt.BigInt)
mintSeveralInitTokens sidechainParams = do
  _ ←
    ( Checkpoint.mintOneCheckpointInitToken
        <> DistributedSet.mintOneDsInitToken
        <> CandidatePermissionToken.mintOneCandidatePermissionInitToken
        <> CommitteeOraclePolicy.mintOneCommitteeOracleInitToken
        <> InitSidechain.initSpendGenesisUtxo
    ) sidechainParams >>= balanceSignAndSubmit "mintSeveralInitTokens"

  pure
    $ foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
    $ map (_ /\ one)
        [ Checkpoint.checkpointInitTokenName
        , DistributedSet.dsInitTokenName
        , CandidatePermissionToken.candidatePermissionInitTokenName
        , CommitteeOraclePolicy.committeeOracleInitTokenName
        ]

-- | Check that `initTokenStatus` gets no tokens if none are
-- | there. Alice initializes the sidechain (and should have one CandidatePermission InitToken).
-- | Then check that Bob has no init tokens.
testInitTokenStatusEmpty ∷ PlutipTest
testInitTokenStatusEmpty =
  Mote.Monad.test "getInitTokenStatus returns empty if no init tokens"
    $ Test.PlutipTest.mkPlutipConfigTest
        ( [ BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          ] /\
            [ BigInt.fromInt 50_000_000
            , BigInt.fromInt 50_000_000
            , BigInt.fromInt 50_000_000
            ]
        )
    $ \(alice /\ bob) → do
        let
          failMsg m = "Should have no init tokens but got: " <> show
            (Plutus.Map.keys m)

        -- Alice init as in testScenario1
        sidechainParams ← Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testInitTokenStatusEmpty'"

          -- Initialize with no CandidatePermission tokens
          initSimpleSidechain Nothing

        -- Bob should have no init tokens
        Wallet.withKeyWallet bob do
          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
            sidechainParams

          Monad.liftContractAffM "Uncreachable"
            $ map Just
            $ assert (failMsg res) (Plutus.Map.null res)

-- | Run `initSidechain` without creating "CandidatePermission" tokens,
-- | and therefore leaving one "CandidatePermission InitToken" unspent.
testInitTokenStatusOneToken ∷ PlutipTest
testInitTokenStatusOneToken =
  Mote.Monad.test
    "getInitTokenStatus returns single init token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testInitTokenStatusOneToken'"

          -- Initialize with no CandidatePermission tokens
          sidechainParams ← initSimpleSidechain Nothing

          -- The CandidatePermission InitToken is the only one unspent
          -- since we did not use it to create any CandidatePermission tokens.
          let
            failMsg m = "Expected a single CandidatePermission InitToken but got: "
              <> show m
            expected = foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
              [ CandidatePermissionToken.candidatePermissionInitTokenName /\ one ]

          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
            sidechainParams

          Monad.liftContractAffM "Uncreachable"
            $ map Just
            $ assert (failMsg res) (res == expected)

-- | Directly mint several init tokens but do not spend them.
-- | Check that you get what you expect.
testInitTokenStatusMultiTokens ∷ PlutipTest
testInitTokenStatusMultiTokens =
  Mote.Monad.test
    "getInitTokenStatus returns expected tokens"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testInitTokenStatusMultiTokens'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          let
            sidechainParams = wrap
              ( (unwrap Test.Utils.dummySidechainParams)
                  { genesisUtxo = genesisUtxo }
              )

          expected ← mintSeveralInitTokens sidechainParams

          let
            -- Plutus.Map.Map's Eq instance is derived from the Array Eq instance
            -- and therefore is sensitive to the order of insertion.
            unorderedEq m1 m2 =
              let
                kvs m = Array.sort $ Array.zip (Plutus.Map.keys m)
                  (Plutus.Map.elems m)
              in
                kvs m1 == kvs m2
            failMsg exp act = "Expected: "
              <> show exp
              <> "\nBut got: "
              <> show act

          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
            sidechainParams

          Monad.liftContractAffM "Uncreachable"
            $ map Just
            $ assert (failMsg expected res) (unorderedEq expected res)
