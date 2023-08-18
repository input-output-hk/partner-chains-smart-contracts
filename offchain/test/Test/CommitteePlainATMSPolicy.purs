module Test.CommitteePlainEcdsaSecp256k1ATMSPolicy
  ( tests
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Transaction (awaitTxConfirmed)
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Data.Traversable (for)
import Mote.Monad as Mote.Monad
import Partial.Unsafe as Unsafe
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  , CommitteeATMSParams(CommitteeATMSParams)
  )
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1Message
  , EcdsaSecp256k1PrivateKey
  , EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

-- | `generateSignatures` generates signatures from the provided committee for
-- | the given `sidechainMessage`
generateSignatures ∷
  { -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , sidechainMessage ∷ EcdsaSecp256k1Message
  } →
  Array (Tuple EcdsaSecp256k1PubKey EcdsaSecp256k1Signature)
generateSignatures
  { currentCommitteePrvKeys
  , sidechainMessage
  } =
  let
    -- Order the private keys by lexicographical ordering of the signatures, so
    -- it's easy to give the sorted pubkey with its associated signature.
    currentCommitteePubKeys /\ currentCommitteePrvKeys' =
      Array.unzip
        $ Array.sortWith fst
        $ map (\prvKey → Utils.Crypto.toPubKeyUnsafe prvKey /\ prvKey)
            currentCommitteePrvKeys

    committeeSignatures = Array.zip
      currentCommitteePubKeys
      (Utils.Crypto.multiSign currentCommitteePrvKeys' sidechainMessage)

  in
    committeeSignatures

-- | `tests` aggregates all the `CommitteePlainEcdsaSecp256k1ATMSPolicy` tests together in
-- | one convenient function.
tests ∷ WrappedTests
tests = plutipGroup "CommitteePlainEcdsaSecp256k1ATMSPolicy minting" $ do
  testScenario1

-- | 'testScenario1' includes various tests for `CommitteePlainEcdsaSecp256k1ATMSPolicy` from
-- | the same sidechain.
testScenario1 ∷ PlutipTest
testScenario1 =
  Mote.Monad.test
    "Various tests for the CommitteePlainEcdsaSecp256k1ATMSPolicy token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← getOwnPaymentPubKeyHash
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        let
          keyCount = 80
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount
          Utils.Crypto.generatePrivKey
        let
          initCommitteePubKeys = map Utils.Crypto.toPubKeyUnsafe
            initCommitteePrvKeys
          initScParams = InitSidechainParams
            { initChainId: BigInt.fromInt 1
            , initGenesisHash: ByteArray.hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: toData $ Utils.Crypto.aggregateKeys
                $ map unwrap initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                pkh
            }

        { sidechainParams, transactionId, versioningTransactionIds } ←
          initSidechain initScParams 1
        awaitTxConfirmed transactionId
        _ ← for versioningTransactionIds awaitTxConfirmed
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
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 0s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 0

            sidechainMessage = Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
              sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              Value.mkTokenName sidechainMessageByteArray

            allPubKeysAndSignatures = generateSignatures
              { -- the current committee stored on chain
                currentCommitteePrvKeys: initCommitteePrvKeys
              , sidechainMessage: sidechainMessage
              }
            committeeSignatures = map (\(pubKey /\ sig) → pubKey /\ Just sig)
              allPubKeysAndSignatures

          -- TODO: CTL updates removed the required functions for `assertMaxFee`,
          -- so this function no longer exists... but perhaps one day we should
          -- get this back. Boo hoo!
          -- ```
          -- Test.Utils.assertMaxFee (BigInt.fromInt 1_000_000)
          -- ```

          utxo ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.runCommitteePlainEcdsaSecp256k1ATMSPolicy
              sidechainParams
              $ CommitteeATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
                  , aggregateSignature: committeeSignatures
                  , message: sidechainMessageTokenName
                  }

          Test.Utils.assertIHaveOutputWithAsset
            committeePlainEcdsaSecp256k1ATMSCurrencySymbol
            sidechainMessageTokenName

        -- the following test cases are mostly duplicated code with slight
        -- variations for the testing
        logInfo'
          "CommitteePlainEcdsaSecp256k1ATMSPolicy a successful mint from the committee with only 54/80 of the committee members"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

            sidechainMessage = Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
              sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              Value.mkTokenName sidechainMessageByteArray

            allPubKeysAndSignatures = generateSignatures
              { currentCommitteePrvKeys: initCommitteePrvKeys
              , sidechainMessage: sidechainMessage
              }

            committeeSignatures =
              ( Array.take 26
                  ( map (\(pubKey /\ _sig) → pubKey /\ Nothing)
                      allPubKeysAndSignatures
                  )
              ) <>
                ( Array.drop 26
                    ( map (\(pubKey /\ sig) → pubKey /\ Just sig)
                        allPubKeysAndSignatures
                    )
                )
          -- note that we have 26 less signatures, so 80 - 26 = 54 out of the
          -- 80 committee members total

          utxo ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.runCommitteePlainEcdsaSecp256k1ATMSPolicy
              sidechainParams
              $ CommitteeATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
                  , aggregateSignature: committeeSignatures
                  , message: sidechainMessageTokenName
                  }

          Test.Utils.assertIHaveOutputWithAsset
            committeePlainEcdsaSecp256k1ATMSCurrencySymbol
            sidechainMessageTokenName

        logInfo'
          "CommitteePlainEcdsaSecp256k1ATMSPolicy a successful mint from the committee where the public keys / signatures are not sorted"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

            sidechainMessage = Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
              sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              Value.mkTokenName sidechainMessageByteArray

            allPubKeysAndSignatures = generateSignatures
              { currentCommitteePrvKeys: initCommitteePrvKeys
              , sidechainMessage: sidechainMessage
              }

            committeeSignatures =
              let
                allPubKeysAndJustSignatures = map
                  (\(pubKey /\ sig) → pubKey /\ Just sig)
                  allPubKeysAndSignatures
              in
                Array.drop 40 allPubKeysAndJustSignatures <> Array.take 40
                  allPubKeysAndJustSignatures

          utxo ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.runCommitteePlainEcdsaSecp256k1ATMSPolicy
              sidechainParams
              $ CommitteeATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
                  , aggregateSignature: committeeSignatures
                  , message: sidechainMessageTokenName
                  }

          Test.Utils.assertIHaveOutputWithAsset
            committeePlainEcdsaSecp256k1ATMSCurrencySymbol
            sidechainMessageTokenName

        logInfo'
          "CommitteePlainEcdsaSecp256k1ATMSPolicy an unsuccessful mint where the committee signs all 2s, but we try to mint all 3s"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 2

            sidechainMessage = Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
              sidechainMessageByteArray

            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust
              $ Value.mkTokenName
              $ ByteArray.byteArrayFromIntArrayUnsafe
              $ Array.replicate 32 3

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
          void
            ( CommitteePlainEcdsaSecp256k1ATMSPolicy.runCommitteePlainEcdsaSecp256k1ATMSPolicy
                sidechainParams
                $ CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
            )
            # Test.Utils.fails

        logInfo'
          "CommitteePlainEcdsaSecp256k1ATMSPolicy an unsuccessful mint where we use wrong committee"
        void do
          wrongCommittee ← sequence $ Array.replicate keyCount
            Utils.Crypto.generatePrivKey
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 4

            sidechainMessage = Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
              sidechainMessageByteArray

            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust
              $ Value.mkTokenName
              $ sidechainMessageByteArray

            allPubKeysAndSignatures = generateSignatures
              { -- the current committee stored on chain
                currentCommitteePrvKeys: wrongCommittee
              , sidechainMessage: sidechainMessage
              }

            committeeSignatures = map (\(pubKey /\ sig) → pubKey /\ Just sig)
              allPubKeysAndSignatures

          utxo ←
            CommitteePlainEcdsaSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          void
            ( CommitteePlainEcdsaSecp256k1ATMSPolicy.runCommitteePlainEcdsaSecp256k1ATMSPolicy
                sidechainParams
                $ CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint: committeePlainEcdsaSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
            )
            # Test.Utils.fails

        pure unit
