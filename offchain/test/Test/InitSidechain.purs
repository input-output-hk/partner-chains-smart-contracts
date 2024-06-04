module Test.InitSidechain
  ( tests
  ) where

import Contract.Prelude

import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum as BigNum
import Contract.Log as Log
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Wallet as Wallet
import Control.Monad.Error.Class as MonadError
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run (liftEffect) as Run
import Run.Except (note) as Run
import Run.Except (throw)
import Test.CandidatePermissionToken as Test.CandidatePermissionToken
import Test.InitSidechain.Utils (failMsg, unorderedEq)
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
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto as Crypto
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
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

-- | `testScenario1` just calls the init sidechain endpoint (which should
-- | succeed!)
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "Calling `initSidechain`"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
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

        initGovernanceAuthority ← (Governance.mkGovernanceAuthority)
          <$> getOwnPaymentPubKeyHash
        let
          initScParams = InitSidechain.InitSidechainParams
            { initChainId: BigInt.fromInt 69
            , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ unsafePartial Crypto.aggregateKeys
                $ map unwrap
                    (map Crypto.toPubKeyUnsafe committeePrvKeys)
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
      ( [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 10_000_000
        ] /\
          [ BigNum.fromInt 50_000_000
          , BigNum.fromInt 50_000_000
          , BigNum.fromInt 50_000_000
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
        initGovernanceAuthority ← (Governance.mkGovernanceAuthority)
          <$> getOwnPaymentPubKeyHash
        let
          initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
          initScParams = InitSidechain.InitSidechainParams
            { initChainId: BigInt.fromInt 69
            , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ unsafePartial Crypto.aggregateKeys
                $ map unwrap
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
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
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
          initGovernanceAuthority ← (Governance.mkGovernanceAuthority)
            <$> getOwnPaymentPubKeyHash
          let
            initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
            initScParams = InitSidechain.InitSidechainParams
              { initChainId: BigInt.fromInt 69
              , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData
                  $ unsafePartial Crypto.aggregateKeys
                  $ map unwrap initCommittee
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initATMSKind: ATMSPlainEcdsaSecp256k1
              , initCandidatePermissionTokenMintInfo: Just one
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
  Maybe BigInt.BigInt →
  Run (APP + BASE + r) SidechainParams.SidechainParams
initSimpleSidechain amt = do
  genesisUtxo ← Test.Utils.getOwnTransactionInput
  -- generate an initialize committee of `committeeSize` committee members
  let committeeSize = 25
  committeePrvKeys ← liftEffect $ sequence $ Array.replicate committeeSize
    Crypto.generatePrivKey

  initGovernanceAuthority ← (Governance.mkGovernanceAuthority)
    <$> getOwnPaymentPubKeyHash
  let
    initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
    initScParams = InitSidechain.InitSidechainParams
      { initChainId: BigInt.fromInt 69
      , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
      , initUtxo: genesisUtxo
      , initAggregatedCommittee: toData $ unsafePartial Crypto.aggregateKeys $ map
          unwrap
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
  Run (APP + r) (Map.Map AssetName BigNum.BigNum)
mintSeveralInitTokens sidechainParams = do
  _ ←
    foldM
      (\acc f → (append acc) <$> f sidechainParams)
      mempty
      [ Checkpoint.mintOneCheckpointInitToken
      , DistributedSet.mintOneDsInitToken
      , CandidatePermissionToken.mintOneCandidatePermissionInitToken
      , CommitteeOraclePolicy.mintOneCommitteeOracleInitToken
      , InitMint.initSpendGenesisUtxo
      ]
      >>= balanceSignAndSubmit "mintSeveralInitTokens"

  pure
    $ foldr (\(k /\ v) → Map.insert k v) Map.empty
    $ map (_ /\ BigNum.fromInt 1)
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
        ( [ BigNum.fromInt 50_000_000
          , BigNum.fromInt 50_000_000
          , BigNum.fromInt 50_000_000
          , BigNum.fromInt 50_000_000
          ] /\
            [ BigNum.fromInt 50_000_000
            , BigNum.fromInt 50_000_000
            , BigNum.fromInt 50_000_000
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
          { initTokenStatusData: res } ← Init.getInitTokenStatus
            sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg "" res) (Map.isEmpty res)

-- | Run `initSidechain` without creating "CandidatePermission" tokens,
-- | and therefore leaving one "CandidatePermission InitToken" unspent.
testInitTokenStatusOneToken ∷ PlutipTest
testInitTokenStatusOneToken =
  Mote.Monad.test
    "getInitTokenStatus returns single init token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    $ \alice → do
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'testInitTokenStatusOneToken'"

          -- Initialize with no CandidatePermission tokens
          sidechainParams ← initSimpleSidechain Nothing

          -- The CandidatePermission InitToken is the only one unspent
          -- since we did not use it to create any CandidatePermission tokens.
          let
            expected = foldr (\(k /\ v) → Map.insert k v) Map.empty
              [ CandidatePermissionToken.candidatePermissionInitTokenName /\
                  BigNum.fromInt 1
              ]

          { initTokenStatusData: res } ← Init.getInitTokenStatus
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
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 40_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
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

          { initTokenStatusData: res } ← Init.getInitTokenStatus
            sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) (unorderedEq expected res)
