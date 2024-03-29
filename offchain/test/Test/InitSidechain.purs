module Test.InitSidechain
  ( tests
  ) where

import Contract.Prelude

import Contract.AssocMap as Plutus.Map
import Contract.Log as Log
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Value as Value
import Contract.Wallet as Wallet
import Control.Monad.Error.Class as MonadError
import Data.Array (toUnfoldable)
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.BigInt as BigInt
import Data.List (List, head)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Mote.Monad as Mote.Monad
import Run (Run)
import Run (liftEffect) as Run
import Run.Except (note) as Run
import Run.Except (throw)
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
import TrustlessSidechain.Effects.App (APP, BASE)
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain as InitSidechain
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto as Crypto
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning (getActualVersionedPoliciesAndValidators)
import TrustlessSidechain.Versioning (getExpectedVersionedPoliciesAndValidators) as Versioning
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Utils (versionOracleInitTokenName) as Versioning
import Type.Row (type (+))

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
  -- InitTokensMint endpoint
  initTokensMintScenario1
  initTokensMintIdempotent
  -- InitCommitteeSelection endpoint
  testInitCommitteeSelection
  testInitCommitteeSelectionUninitialised
  testInitCommitteeSelectionIdempotent
  -- InitCheckpoint endpoint
  testInitCheckpointUninitialised
  testInitCheckpoint
  testInitCheckpointIdempotent
  -- InitFuel endpoint
  initFuelSucceeds
  initFuelIdempotent

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
      withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ Log.logInfo' "InitSidechain 'testScenario1'"
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        -- generate an initialize committee of `committeeSize` committee members
        let committeeSize = 25
        committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          committeeSize
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
      aliceUtxos ← withUnliftApp (Wallet.withKeyWallet alice) $
        Effect.fromMaybeThrow
          (GenericInternalError "Failed to query wallet utxos")
          (liftContract Wallet.getWalletUtxos)

      genesisUtxo ←
        Run.note (GenericInternalError "No utxo found in wallet")
          $ Set.findMin
          $ Map.keys aliceUtxos

      result ← withUnliftApp (MonadError.try <<< Wallet.withKeyWallet bob) do
        -- generate an initialize committee of `committeeSize` committee members
        let committeeSize = 1000
        committeePrvKeys ← liftEffect $ sequence $ Array.replicate
          committeeSize
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
          throw $ GenericInternalError
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
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'testScenario3'"
          genesisUtxo ← Test.Utils.getOwnTransactionInput
          -- generate an initialize committee of `committeeSize` committee members
          let committeeSize = 25
          committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
            committeeSize
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
  ∀ r.
  Maybe { candidatePermissionTokenAmount ∷ BigInt.BigInt } →
  Run (APP + BASE + r) SidechainParams.SidechainParams
initSimpleSidechain amt = do
  genesisUtxo ← Test.Utils.getOwnTransactionInput
  -- generate an initialize committee of `committeeSize` committee members
  let committeeSize = 25
  committeePrvKeys ← liftEffect $ sequence $ Array.replicate committeeSize
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
  ∀ r.
  SidechainParams →
  Run (APP + r) (Plutus.Map.Map Value.TokenName BigInt.BigInt)
mintSeveralInitTokens sidechainParams = do
  _ ←
    foldM
      (\acc f → (append acc) <$> f sidechainParams)
      mempty
      [ Checkpoint.mintOneCheckpointInitToken
      , DistributedSet.mintOneDsInitToken
      , CandidatePermissionToken.mintOneCandidatePermissionInitToken
      , CommitteeOraclePolicy.mintOneCommitteeOracleInitToken
      , InitSidechain.initSpendGenesisUtxo
      ]
      >>= balanceSignAndSubmit "mintSeveralInitTokens"

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

        -- Alice init as in testScenario1
        sidechainParams ← withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'testInitTokenStatusEmpty'"

          -- Initialize with no CandidatePermission tokens
          initSimpleSidechain Nothing

        -- Bob should have no init tokens
        withUnliftApp (Wallet.withKeyWallet bob) do
          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
            sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg "" res) (Plutus.Map.null res)

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
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'testInitTokenStatusOneToken'"

          -- Initialize with no CandidatePermission tokens
          sidechainParams ← initSimpleSidechain Nothing

          -- The CandidatePermission InitToken is the only one unspent
          -- since we did not use it to create any CandidatePermission tokens.
          let
            expected = foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
              [ CandidatePermissionToken.candidatePermissionInitTokenName /\ one ]

          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
            sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg "Single CandidatePermission InitToken" res)
                (res == expected)

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
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'testInitTokenStatusMultiTokens'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          let
            sidechainParams = wrap
              ( (unwrap Test.Utils.dummySidechainParams)
                  { genesisUtxo = genesisUtxo }
              )

          expected ← mintSeveralInitTokens sidechainParams

          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
            sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) (unorderedEq expected res)

-- | Setup the same as `testScenario1`, but in which `initTokensMint`
-- | is called rather than `initSidechain`. It should succeed, and
-- | it checks the tokens are indeed minted by calling `getInitTokenStatus`.
initTokensMintScenario1 ∷ PlutipTest
initTokensMintScenario1 =
  Mote.Monad.test "`initTokensMint` returns expected token names and quantities"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initTokensMintScenario1'"

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

          -- Command being tested
          void $ InitSidechain.initTokensMint sidechainParams initATMSKind version

          -- For computing the number of versionOracle init tokens
          { versionedPolicies, versionedValidators } ←
            Versioning.getExpectedVersionedPoliciesAndValidators
              { atmsKind: initATMSKind
              , sidechainParams
              }
              version

          let
            -- See `Versioning.mintVersionInitTokens` for where this comes from
            nversion = BigInt.fromInt $ List.length versionedPolicies
              + List.length versionedValidators
            expected = expectedInitTokens nversion

          -- Get the tokens just created
          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
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
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initTokensMintIdempotent'"

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

          -- Mint them once
          void $ InitSidechain.initTokensMint sidechainParams
            initATMSKind
            version

          -- Then do it again.
          { transactionId } ← InitSidechain.initTokensMint sidechainParams
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
            -- See `Versioning.mintVersionInitTokens` for where this comes from
            nversion = BigInt.fromInt $ List.length versionedPolicies
              + List.length versionedValidators
            expected = expectedInitTokens nversion

          -- Get the tokens just created
          { initTokenStatusData: res } ← InitSidechain.getInitTokenStatus
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

-- | Setup the same as `testScenario1`, but in which `initTokensMint`
-- | is called followed by `initFuel`.
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
          Effect.logInfo' "InitSidechain 'initFuelScenario1'"

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
          void $ InitSidechain.initTokensMint sidechainParams initATMSKind version

          fuelRes ← InitSidechain.initFuel sidechainParams initATMSKind version

          -- Was the DsInitToken burned?
          { initTokenStatusData: tokenStatus } ← InitSidechain.getInitTokenStatus
            sidechainParams

          let
            expected = true
            dsSpent = not $
              Plutus.Map.member
                DistributedSet.dsInitTokenName
                tokenStatus
            res = isJust fuelRes && dsSpent

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) res

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
          void $ InitSidechain.initTokensMint sidechainParams initATMSKind version

          void $ InitSidechain.initFuel sidechainParams initATMSKind version

          -- Second call should do nothing.
          fuelRes ← InitSidechain.initFuel sidechainParams initATMSKind version

          let
            expected = true
            res = isNothing fuelRes

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) res

-- | Testing utility to check ordered equality of
-- | Plutus.Map.Map, whose Eq instance is derived from the Array Eq instance
-- | and therefore is sensitive to the order of insertion.
-- | Note this is not *set* equality, since there is no deduplication.
unorderedEq ∷
  ∀ k v.
  Ord k ⇒
  Ord v ⇒
  Plutus.Map.Map k v →
  Plutus.Map.Map k v →
  Boolean
unorderedEq m1 m2 =
  let
    kvs m = Array.sort $ Array.zip (Plutus.Map.keys m)
      (Plutus.Map.elems m)
  in
    kvs m1 == kvs m2

-- | Testing utility for showing expected/actual
failMsg ∷ ∀ a b. Show a ⇒ Show b ⇒ a → b → String
failMsg exp act = "Expected: "
  <> show exp
  <> "\nBut got: "
  <> show act

-- | Collection of init tokens expected to be minted by
-- | `initTokensMint`. It does not care about
-- | ATMSKinds or the particular version, just the token name
-- | and quantity. Requires the number of version oracle init tokens
-- | to be passed.
expectedInitTokens ∷
  BigInt.BigInt →
  Plutus.Map.Map Value.TokenName BigInt.BigInt
expectedInitTokens nversion =
  foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
    $ Array.(:) (Versioning.versionOracleInitTokenName /\ nversion)
    $
      map
        (_ /\ one)
        [ Checkpoint.checkpointInitTokenName
        , DistributedSet.dsInitTokenName
        , CommitteeOraclePolicy.committeeOracleInitTokenName
        , CandidatePermissionToken.candidatePermissionInitTokenName
        ]

-- | Test `InitCheckpoint` without having run `initTokensMint`, expecting failure
testInitCheckpointUninitialised ∷ PlutipTest
testInitCheckpointUninitialised =
  Mote.Monad.test "Calling `InitCheckpoint` with no init token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        result ← withUnliftApp (MonadError.try <<< Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCheckpointUninitialised'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              initGenesisHash = ByteArray.hexToByteArrayUnsafe "abababababa"
              initCandidatePermissionTokenMintInfo = Nothing
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitSidechain.initCheckpoint sidechainParams
              initCandidatePermissionTokenMintInfo
              initGenesisHash
              initATMSKind
              1
        case result of
          Right _ →
            throw $ GenericInternalError
              "Contract should have failed but it didn't."
          Left _err → pure unit

-- | Test `InitCheckpoint` having run `initTokensMint`, expecting success and for the
-- | `checkpointInitToken` to be spent
testInitCheckpoint ∷ PlutipTest
testInitCheckpoint =
  Mote.Monad.test "Calling `InitCheckpoint`"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCheckpoint'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initGenesisHash = ByteArray.hexToByteArrayUnsafe "abababababa"
              initCandidatePermissionTokenMintInfo = Nothing
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitSidechain.initTokensMint sidechainParams
              initATMSKind
              version

            void $ InitSidechain.initCheckpoint sidechainParams
              initCandidatePermissionTokenMintInfo
              initGenesisHash
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
              -- See `Versioning.mintVersionInitTokens` for where this comes from
              nversion = BigInt.fromInt $ List.length versionedPolicies
                + List.length versionedValidators
              expectedTokens =
                foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
                  $ Array.(:)
                      ( Versioning.versionOracleInitTokenName /\
                          (nversion - fromInt 1)
                      )
                  $
                    map
                      (_ /\ one)
                      [ DistributedSet.dsInitTokenName
                      , CommitteeOraclePolicy.committeeOracleInitTokenName
                      , CandidatePermissionToken.candidatePermissionInitTokenName
                      ]

            -- Get the tokens just created
            { initTokenStatusData: resTokens } ← InitSidechain.getInitTokenStatus
              sidechainParams

            { versionedValidators: validatorsRes } ←
              getActualVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedExistingValidator = Just CheckpointValidator
              actualExistingValidator = head $ map fst validatorsRes

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $ assert (failMsg expectedTokens resTokens)
                  (unorderedEq expectedTokens resTokens)
              <* assert
                ( failMsg expectedExistingValidator
                    actualExistingValidator
                )
                ( expectedExistingValidator ==
                    actualExistingValidator
                )

-- | Test running `initCheckpoint` twice, having run `initTokensMint`, expecting idempotency
-- | and for the `checkpointInitToken` to be spent
testInitCheckpointIdempotent ∷ PlutipTest
testInitCheckpointIdempotent =
  Mote.Monad.test "Calling `InitCheckpoint` twice, expecting idempotency"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCheckpointIdempotent'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initGenesisHash = ByteArray.hexToByteArrayUnsafe "abababababa"
              initCandidatePermissionTokenMintInfo = Nothing
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            -- Initialise tokens
            void $ InitSidechain.initTokensMint sidechainParams
              initATMSKind
              version

            -- Initialise checkpoint
            void $ InitSidechain.initCheckpoint sidechainParams
              initCandidatePermissionTokenMintInfo
              initGenesisHash
              initATMSKind
              version

            -- Then do it again
            res ← InitSidechain.initCheckpoint sidechainParams
              initCandidatePermissionTokenMintInfo
              initGenesisHash
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
              -- See `Versioning.mintVersionInitTokens` for where this comes from
              nversion = BigInt.fromInt $ List.length versionedPolicies
                + List.length versionedValidators
              expectedTokens =
                foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
                  $ Array.(:)
                      ( Versioning.versionOracleInitTokenName /\
                          (nversion - fromInt 1)
                      )
                  $
                    map
                      (_ /\ one)
                      [ DistributedSet.dsInitTokenName
                      , CommitteeOraclePolicy.committeeOracleInitTokenName
                      , CandidatePermissionToken.candidatePermissionInitTokenName
                      ]

            -- Get the tokens just created
            { initTokenStatusData: resTokens } ← InitSidechain.getInitTokenStatus
              sidechainParams

            { versionedValidators: validatorsRes } ←
              getActualVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedExistingValidator = Just CheckpointValidator
              actualExistingValidator = head $ map fst validatorsRes

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $ assert (failMsg expectedTokens resTokens)
                  (unorderedEq expectedTokens resTokens)
              <* assert (failMsg "Nothing" res) (isNothing res)
              <* assert
                ( failMsg expectedExistingValidator
                    actualExistingValidator
                )
                ( expectedExistingValidator ==
                    actualExistingValidator
                )

-- | Test `initCommitteeSelection` having run `initTokensMint`, expecting success and for the
-- | the relevant tokens to be spent
testInitCommitteeSelection ∷ PlutipTest
testInitCommitteeSelection =
  Mote.Monad.test "Calling `InitCommitteeSelection`"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCommitteeSelection'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            -- generate an initialize committee of `committeeSize` committee members
            let committeeSize = 25
            committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
              committeeSize
              Crypto.generatePrivKey

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
              initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
                unwrap
                initCommittee
              initCandidatePermissionTokenMintInfo = Nothing
              initSidechainEpoch = zero
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitSidechain.initTokensMint sidechainParams
              initATMSKind
              version
            void $ InitSidechain.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
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
              -- See `Versioning.mintVersionInitTokens` for where this comes from
              nversion = BigInt.fromInt $ List.length versionedPolicies
                + List.length versionedValidators
              expectedTokens =
                foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
                  $ Array.(:)
                      ( Versioning.versionOracleInitTokenName /\
                          (nversion - fromInt 4)
                      )
                  $
                    map
                      (_ /\ one)
                      [ DistributedSet.dsInitTokenName
                      , Checkpoint.checkpointInitTokenName
                      , CandidatePermissionToken.candidatePermissionInitTokenName
                      ]

            -- Get the tokens just created
            { initTokenStatusData: resTokens } ← InitSidechain.getInitTokenStatus
              sidechainParams

            { versionedValidators: validatorsRes
            , versionedPolicies: policiesRes
            } ←
              getActualVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedExistingValidators =
                [ CommitteeHashValidator
                , CommitteeCandidateValidator
                ]
              expectedExistingPolicies =
                [ CommitteeCertificateVerificationPolicy, CommitteeOraclePolicy ]
              actualExistingValidators = map fst validatorsRes
              actualExistingPolicies = map fst policiesRes

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $ assert (failMsg expectedTokens resTokens)
                  (unorderedEq expectedTokens resTokens)
              <* assert
                ( failMsg expectedExistingValidators
                    actualExistingValidators
                )
                ( actualExistingValidators `sublist` toUnfoldable
                    expectedExistingValidators
                )
              <* assert
                ( failMsg expectedExistingPolicies
                    actualExistingPolicies
                )
                ( actualExistingPolicies `sublist` toUnfoldable
                    expectedExistingPolicies
                )

-- | Test `initCommitteeSelection` without having run `initTokensMint`, expecting failure
testInitCommitteeSelectionUninitialised ∷ PlutipTest
testInitCommitteeSelectionUninitialised =
  Mote.Monad.test "Calling `InitCommitteeSelection` with no init token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        result ← withUnliftApp (MonadError.try <<< Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCommitteeSelectionUninitialised'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            -- generate an initialize committee of `committeeSize` committee members
            let committeeSize = 25
            committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
              committeeSize
              Crypto.generatePrivKey

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
              initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
                unwrap
                initCommittee
              initCandidatePermissionTokenMintInfo = Nothing
              initSidechainEpoch = zero
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitSidechain.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              1
        case result of
          Right _ →
            throw $ GenericInternalError
              "Contract should have failed but it didn't."
          Left _err → pure unit

-- | Test running `initCommitteeSelection` twice, having run `initTokensMint`, expecting idempotency
-- | and for the relevant tokens to be spent
testInitCommitteeSelectionIdempotent ∷ PlutipTest
testInitCommitteeSelectionIdempotent =
  Mote.Monad.test "Calling `InitCommitteeSelection` twice, expecting idempotency"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice)
          do
            liftContract $ Log.logInfo'
              "InitSidechain 'testInitCommitteeSelectionIdempotent'"
            genesisUtxo ← Test.Utils.getOwnTransactionInput
            -- generate an initialize committee of `committeeSize` committee members
            let committeeSize = 25
            committeePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
              committeeSize
              Crypto.generatePrivKey

            initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash
            let
              version = 1
              initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
              initAggregatedCommittee = toData $ Crypto.aggregateKeys $ map
                unwrap
                initCommittee
              initCandidatePermissionTokenMintInfo = Nothing
              initSidechainEpoch = zero
              initATMSKind = ATMSPlainEcdsaSecp256k1
              sidechainParams = SidechainParams.SidechainParams
                { chainId: BigInt.fromInt 9
                , genesisUtxo: genesisUtxo
                , thresholdNumerator: BigInt.fromInt 2
                , thresholdDenominator: BigInt.fromInt 3
                , governanceAuthority: initGovernanceAuthority
                }

            void $ InitSidechain.initTokensMint sidechainParams
              initATMSKind
              version
            void $ InitSidechain.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
              initATMSKind
              version
            res ← InitSidechain.initCommitteeSelection sidechainParams
              initCandidatePermissionTokenMintInfo
              initSidechainEpoch
              initAggregatedCommittee
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
              -- See `Versioning.mintVersionInitTokens` for where this comes from
              nversion = BigInt.fromInt $ List.length versionedPolicies
                + List.length versionedValidators
              expectedTokens =
                foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
                  $ Array.(:)
                      ( Versioning.versionOracleInitTokenName /\
                          (nversion - fromInt 4)
                      )
                  $
                    map
                      (_ /\ one)
                      [ DistributedSet.dsInitTokenName
                      , Checkpoint.checkpointInitTokenName
                      , CandidatePermissionToken.candidatePermissionInitTokenName
                      ]

            -- Get the tokens just created
            { initTokenStatusData: resTokens } ← InitSidechain.getInitTokenStatus
              sidechainParams

            { versionedValidators: validatorsRes, versionedPolicies: policiesRes } ←
              getActualVersionedPoliciesAndValidators
                { atmsKind: initATMSKind
                , sidechainParams
                }
                version

            let
              expectedExistingValidators =
                [ CommitteeHashValidator
                , CommitteeCandidateValidator
                ]
              expectedExistingPolicies =
                [ CommitteeCertificateVerificationPolicy
                , CommitteeOraclePolicy
                ]
              actualExistingValidators = map fst validatorsRes
              actualExistingPolicies = map fst policiesRes

            Effect.fromMaybeThrow (GenericInternalError "Unreachable")
              $ map Just
              $ liftAff
              $ assert (failMsg expectedTokens resTokens)
                  (unorderedEq expectedTokens resTokens)
              <* assert (failMsg "Nothing" res) (isNothing res)
              <* assert
                ( failMsg expectedExistingValidators
                    actualExistingValidators
                )
                ( actualExistingValidators `sublist`
                    toUnfoldable expectedExistingValidators
                )
              <* assert
                ( failMsg expectedExistingPolicies
                    actualExistingPolicies
                )
                ( actualExistingPolicies `sublist` toUnfoldable
                    expectedExistingPolicies
                )

sublist ∷ ∀ a. Eq a ⇒ List a → List a → Boolean
sublist listA listB = all (\b → b `elem` listA) listB
