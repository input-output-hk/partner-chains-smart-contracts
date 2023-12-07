module Test.GarbageCollector where

import Contract.Prelude

import Contract.Address (pubKeyHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Prim.ByteArray as ByteArray
import Contract.Transaction (awaitTxConfirmed)
import Contract.Value (TokenName)
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Partial.Unsafe as Unsafe
import Test.CommitteePlainEcdsaSecp256k1ATMSPolicy (generateSignatures)
import Test.MerkleRoot as Test.MerkleRoot
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils
  ( WrappedTests
  , fails
  , getOwnTransactionInput
  , plutipGroup
  )
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  , CommitteeATMSParams(CommitteeATMSParams)
  )
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.FUELBurningPolicy.V1 as BurningV1
import TrustlessSidechain.FUELBurningPolicy.V2 as BurningV2
import TrustlessSidechain.FUELMintingPolicy.V1
  ( MerkleTreeEntry(MerkleTreeEntry)
  )
import TrustlessSidechain.FUELMintingPolicy.V1 as MintingV1
import TrustlessSidechain.FUELMintingPolicy.V2 as MintingV2
import TrustlessSidechain.GarbageCollector as GarbageCollector
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PrivateKey
  , aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning as Versioning

-- | `tests` aggregate all the FUELMintingPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = plutipGroup "Burning waste NFTs using GarbageCollector"
  $ do
      testScenarioSuccess

testScenarioSuccess ∷ PlutipTest
testScenarioSuccess =
  Mote.Monad.test "Mint atms, fuel mint and fuel burn tokens, then burn them all"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 150_000_000, BigInt.fromInt 150_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        { sidechainParams, initCommitteePrvKeys } ← initializeSidechain
        atmsTokenName ← mintATMSTokens { sidechainParams, initCommitteePrvKeys }
        mintFuelMintingAndFuelBurningTokens
          { sidechainParams, initCommitteePrvKeys }

        void
          $ GarbageCollector.mkBurnNFTsLookupsAndConstraints sidechainParams
          >>= balanceSignAndSubmit "Test: burn NFTs"

        committeePlainEcdsaSecp256k1ATMSMint ←
          CommitteePlainEcdsaSecp256k1ATMSPolicy.committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams
            sidechainParams

        { committeePlainEcdsaSecp256k1ATMSCurrencySymbol } ←
          CommitteePlainEcdsaSecp256k1ATMSPolicy.getCommitteePlainEcdsaSecp256k1ATMSPolicy
            { committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
            , sidechainParams
            }

        { fuelMintingCurrencySymbol } ← MintingV1.getFuelMintingPolicy
          sidechainParams
        { fuelBurningCurrencySymbol } ← BurningV1.getFuelBurningPolicy
          sidechainParams

        Test.Utils.assertIHaveOutputWithAsset
          fuelMintingCurrencySymbol
          MintingV1.fuelTokenName
          # fails

        Test.Utils.assertIHaveOutputWithAsset
          fuelBurningCurrencySymbol
          BurningV1.fuelTokenName
          # fails

        Test.Utils.assertIHaveOutputWithAsset
          committeePlainEcdsaSecp256k1ATMSCurrencySymbol
          atmsTokenName
          # fails

initializeSidechain ∷
  Contract
    { sidechainParams ∷ SidechainParams
    , initCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
    }
initializeSidechain = do
  pkh ← getOwnPaymentPubKeyHash
  genesisUtxo ← getOwnTransactionInput
  let
    keyCount = 25
  initCommitteePrvKeys ← sequence $ Array.replicate keyCount generatePrivKey
  let
    initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
    initScParams = InitSidechainParams
      { initChainId: BigInt.fromInt 1
      , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
      , initUtxo: genesisUtxo
      , initAggregatedCommittee: PlutusData.toData $ aggregateKeys
          $ map unwrap
              initCommitteePubKeys
      , initSidechainEpoch: zero
      , initThresholdNumerator: BigInt.fromInt 2
      , initThresholdDenominator: BigInt.fromInt 3
      , initCandidatePermissionTokenMintInfo: Nothing
      , initATMSKind: ATMSPlainEcdsaSecp256k1
      , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap pkh
      }

  { sidechainParams, transactionId, versioningTransactionIds } ←
    initSidechain initScParams 1
  awaitTxConfirmed transactionId
  _ ← for versioningTransactionIds awaitTxConfirmed

  versioningTransactionIds2 ← Versioning.insertVersion
    { sidechainParams, atmsKind: ATMSPlainEcdsaSecp256k1 }
    2
  _ ← for versioningTransactionIds2 awaitTxConfirmed

  pure { sidechainParams, initCommitteePrvKeys }

mintATMSTokens ∷
  { sidechainParams ∷ SidechainParams
  , initCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  } →
  Contract TokenName
mintATMSTokens { sidechainParams, initCommitteePrvKeys } = do
  -- Grabbing the CommitteePlainEcdsaSecp256k1ATMSPolicy on chain parameters / minting policy
  -------------------------
  committeePlainEcdsaSecp256k1ATMSMint ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams
      sidechainParams

  { committeePlainEcdsaSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.getCommitteePlainEcdsaSecp256k1ATMSPolicy
      { committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
      , sidechainParams
      }

  -- Running the tests
  -------------------------
  logInfo'
    "CommitteePlainEcdsaSecp256k1ATMSPolicy a successful mint from the committee"
  let
    sidechainMessageByteArray =
      -- byte array of 32 bytes which are all 0s.
      ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 0

    sidechainMessage = Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
      sidechainMessageByteArray
    sidechainMessageTokenName =
      Unsafe.unsafePartial $ Maybe.fromJust $
        Value.mkTokenName sidechainMessageByteArray

    allPubKeysAndSignatures = generateSignatures
      { -- the current committee stored on chain
        currentCommitteePrvKeys: initCommitteePrvKeys
      , sidechainMessage: sidechainMessage
      }
    committeeSignatures = map (\(pubKey /\ sig) → pubKey /\ Just sig)
      allPubKeysAndSignatures

  utxo ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
      sidechainParams
  txHash ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.runCommitteePlainEcdsaSecp256k1ATMSPolicy
      sidechainParams
      $ CommitteeATMSParams
          { currentCommitteeUtxo: utxo
          , committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
          , aggregateSignature: committeeSignatures
          , message: sidechainMessageTokenName
          }

  awaitTxConfirmed txHash

  Test.Utils.assertIHaveOutputWithAsset
    committeePlainEcdsaSecp256k1ATMSCurrencySymbol
    sidechainMessageTokenName

  pure sidechainMessageTokenName

mintFuelMintingAndFuelBurningTokens ∷
  { sidechainParams ∷ SidechainParams
  , initCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  } →
  Contract Unit
mintFuelMintingAndFuelBurningTokens { sidechainParams, initCommitteePrvKeys } =
  do
    pkh ← getOwnPaymentPubKeyHash
    ownRecipient ←
      liftContractM "Could not convert pub key hash to bech 32 bytes" $
        Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
    let
      amount = BigInt.fromInt 5
      recipient = pubKeyHashAddress pkh Nothing
      index = BigInt.fromInt 0
      previousMerkleRoot = Nothing
      ownEntry =
        MerkleTreeEntry
          { index
          , amount
          , previousMerkleRoot
          , recipient: ownRecipient
          }

      ownEntryBytes = unwrap $ PlutusData.serializeData ownEntry
      merkleTree =
        unsafePartial $ fromJust $ hush $ MerkleTree.fromArray
          [ ownEntryBytes ]

      merkleProof = unsafePartial $ fromJust $ MerkleTree.lookupMp ownEntryBytes
        merkleTree
    void $ Test.MerkleRoot.saveRoot
      { sidechainParams
      , merkleTreeEntries: [ ownEntry ]
      , currentCommitteePrvKeys: initCommitteePrvKeys
      , previousMerkleRoot: Nothing
      }

    void
      $
        ( MintingV1.mkMintFuelLookupAndConstraints sidechainParams $
            MintingV1.FuelMintParams
              { amount
              , recipient
              , sidechainParams
              , merkleProof
              , index
              , previousMerkleRoot
              , dsUtxo: Nothing
              }
        )
      >>= balanceSignAndSubmit "Test: mint V1 fuel"

    void
      $ BurningV1.mkBurnFuelLookupAndConstraints
          (BurningV1.FuelBurnParams { sidechainParams, amount })
      >>=
        balanceSignAndSubmit "Test: burn V1 fuel"

    void
      $
        ( MintingV2.mkMintFuelLookupAndConstraints sidechainParams $
            MintingV2.FuelMintParams
              { amount
              }
        )
      >>= balanceSignAndSubmit "Test: mint V2 fuel"

    void
      $ BurningV2.mkBurnFuelLookupAndConstraints
          (BurningV2.FuelBurnParams { sidechainParams, amount })
      >>=
        balanceSignAndSubmit "Test: burn V2 fuel"

    { fuelMintingCurrencySymbol
    } ← MintingV1.getFuelMintingPolicy sidechainParams

    { fuelBurningCurrencySymbol
    } ← BurningV1.getFuelBurningPolicy sidechainParams

    { fuelMintingCurrencySymbol: fuelMintingCurrencySymbol2
    } ← MintingV2.getFuelMintingPolicy sidechainParams

    { fuelBurningCurrencySymbol: fuelBurningCurrencySymbol2
    } ← BurningV2.getFuelBurningPolicy sidechainParams

    Test.Utils.assertIHaveOutputWithAsset
      fuelMintingCurrencySymbol
      MintingV1.fuelTokenName

    Test.Utils.assertIHaveOutputWithAsset
      fuelBurningCurrencySymbol
      BurningV1.fuelTokenName

    Test.Utils.assertIHaveOutputWithAsset
      fuelMintingCurrencySymbol2
      MintingV2.dummyTokenName

    Test.Utils.assertIHaveOutputWithAsset
      fuelBurningCurrencySymbol2
      BurningV2.dummyTokenName
