module Test.InitSidechain
  ( generateInitCommittee
  , testScenario1
  , testScenario2
  , tests
  ) where

import Contract.Prelude

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
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain as InitSidechain
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PrivateKey
  , EcdsaSecp256k1PubKey
  )
import TrustlessSidechain.Utils.Crypto as Crypto

-- | `tests` aggregates all the tests together in one convenient funciton
tests ∷ WrappedTests
tests = plutipGroup "Initialising the sidechain" $ do
  testScenario1
  testScenario2
  testScenario3
  testScenario4
  testScenario5

-- | `generateInitCommittee` generates an intial committee of the given size as
-- | an array of tuple associating public key to private key.
-- |
-- | Note: this also sorts the inital committee lexicographically by the public
-- | keys (this technically isn't needed since the endpoint will do it for you)
-- This may be helpful when attempting to use the CLI interface to generate
-- test cases manually.
generateInitCommittee ∷
  Int → Effect (Array (EcdsaSecp256k1PubKey /\ EcdsaSecp256k1PrivateKey))
generateInitCommittee committeeSize = do
  committeePrvKeys ← sequence $ Array.replicate committeeSize
    Crypto.generateRandomPrivateKey
  pure $ Array.sortWith fst $ map
    (\prvKey → Crypto.toPubKeyUnsafe prvKey /\ prvKey)
    committeePrvKeys

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

-- | `testScenario2` initalizes the sidechain endpoint in two parts:
-- |
-- |      1. Calling `InitSidechain.initSidechainTokens`
-- |
-- |      2. Calling `InitSidechain.initCommittee`
-- |
-- | Otherwise, this is mostly the same as `testScenario1`
-- | See issue #174.
testScenario2 ∷ PlutipTest
testScenario2 =
  Mote.Monad.test "Calling `initSidechain` as the two step process"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 20_000_000
        ]
    $ \alice →
        Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testScenario2'"
          genesisUtxo ← Test.Utils.getOwnTransactionInput
          -- generate an initialize committee of @committeeSize@ committee members
          let committeeSize = 25
          committeePrvKeys ← sequence $ Array.replicate committeeSize
            Crypto.generatePrivKey
          initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
            <$> getOwnPaymentPubKeyHash
          let
            initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
            initScParams =
              { initChainId: BigInt.fromInt 69
              , initGenesisHash: ByteArray.hexToByteArrayUnsafe "abababababa"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: toData $ Crypto.aggregateKeys
                  $ map unwrap initCommittee
              , initSidechainEpoch: zero
              , initATMSKind: ATMSPlainEcdsaSecp256k1
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority
              }

          void do
            _sc ← InitSidechain.initSidechainTokens initScParams 1
            InitSidechain.paySidechainTokens initScParams 1

-- | `testScenario3` is a bit more complicated (but this should fail!) than
-- | `testScenario2`. It takes two distinct wallets, say Alice and Bob, grabs a
-- | utxo from Alice as the `initUtxo` (`genesisUtxo`); then Bob tries to
-- | initialize the sidechain with Alice's utxo.
-- | In short, this verifies that to initialize the sidechain, we need to spend
-- | the `initUtxo`
testScenario3 ∷ PlutipTest
testScenario3 = Mote.Monad.test "Verifying `initSidechain` spends `initUtxo`"
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

-- | `testScenario4` is identical to `testScenario1` BUT we include the minting
-- | of candidate permission tokens, and verify that we actually have the
-- | candidate permission token afterwards
testScenario4 ∷ PlutipTest
testScenario4 =
  Mote.Monad.test "Calling `initSidechain` with candidate permission tokens"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice →
        Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testScenario4'"
          genesisUtxo ← Test.Utils.getOwnTransactionInput
          -- generate an initialize committee of `committeeSize` committee members
          let committeeSize = 25
          committeePrvKeys ← sequence $ Array.replicate committeeSize
            Crypto.generatePrivKey
          initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
            <$> getOwnPaymentPubKeyHash
          let
            permissionToken =
              { candidatePermissionTokenUtxo: genesisUtxo
              , candidatePermissionTokenName: Value.adaToken
              }
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
                    { amount: one
                    , permissionToken
                    }
              , initGovernanceAuthority
              }

          { sidechainParams: sc } ← InitSidechain.initSidechain initScParams 1
          Test.CandidatePermissionToken.assertIHaveCandidatePermissionToken sc
            permissionToken

-- | `testScenario5` is identical to `testScenario2` BUT we include the minting
-- | of candidate permission tokens, and verify that we actually have the
-- | candidate permission token afterwards
testScenario5 ∷ PlutipTest
testScenario5 = do
  Mote.Monad.test
    "Calling `initSidechain` as the two step process with candidate permission tokens"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 10_000_000
        ]
    $ \alice →
        Wallet.withKeyWallet alice do
          Log.logInfo' "InitSidechain 'testScenario2'"
          genesisUtxo ← Test.Utils.getOwnTransactionInput
          -- generate an initialize committee of @committeeSize@ committee members
          let committeeSize = 25
          committeePrvKeys ← sequence $ Array.replicate committeeSize
            Crypto.generatePrivKey
          initGovernanceAuthority ← (Governance.mkGovernanceAuthority <<< unwrap)
            <$> getOwnPaymentPubKeyHash
          let
            initCommittee = map Crypto.toPubKeyUnsafe committeePrvKeys
            permissionToken =
              { candidatePermissionTokenUtxo: genesisUtxo
              , candidatePermissionTokenName: Value.adaToken
              }
            initScParams =
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
                    { amount: one
                    , permissionToken
                    }
              , initGovernanceAuthority
              }

          void do
            { sidechainParams: sc } ←
              InitSidechain.initSidechainTokens initScParams 1
            Test.CandidatePermissionToken.assertIHaveCandidatePermissionToken sc
              permissionToken
            InitSidechain.paySidechainTokens initScParams 1
