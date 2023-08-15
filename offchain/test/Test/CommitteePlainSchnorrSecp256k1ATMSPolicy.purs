-- | `Test.CommitteePlainSchnorrSecp256k1ATMSPolicy` includes tests for the
-- | plain schnorr secp2256k1 policy.
-- | Warning: this is mostly duplicated from `Test.CommitteePlainEcdsaSecp256k1ATMSPolicy`.
module Test.CommitteePlainSchnorrSecp256k1ATMSPolicy
  ( tests
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Effect.Class as Effect.Class
import Mote.Monad as Mote.Monad
import Partial.Unsafe as Unsafe
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds(ATMSPlainSchnorrSecp256k1)
  , CommitteeATMSParams(CommitteeATMSParams)
  )
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SchnorrSecp256k1
  ( SchnorrSecp256k1PrivateKey
  , SchnorrSecp256k1PublicKey
  , SchnorrSecp256k1Signature
  )
import TrustlessSidechain.Utils.SchnorrSecp256k1 as Utils.SchnorrSecp256k1

-- | `generateSignatures` generates signatures from the provided committee for
-- | the given `sidechainMessage`
generateSignatures ∷
  { -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array SchnorrSecp256k1PrivateKey
  , sidechainMessage ∷ ByteArray
  } →
  Array (Tuple SchnorrSecp256k1PublicKey SchnorrSecp256k1Signature)
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
        $ map (\prvKey → Utils.SchnorrSecp256k1.toPubKey prvKey /\ prvKey)
            currentCommitteePrvKeys

    committeeSignatures = Array.zip
      currentCommitteePubKeys
      ( map
          (Utils.SchnorrSecp256k1.sign sidechainMessage)
          currentCommitteePrvKeys'
      )

  in
    committeeSignatures

-- | `tests` aggregates all the `CommitteePlainSchnorrSecp256k1ATMSPolicy` tests together in
-- | one convenient function.
tests ∷ WrappedTests
tests = plutipGroup "CommitteePlainSchnorrSecp256k1ATMSPolicy minting" $ do
  testScenario1

-- | 'testScenario1' includes various tests for `CommitteePlainSchnorrSecp256k1ATMSPolicy` from
-- | the same sidechain.
testScenario1 ∷ PlutipTest
testScenario1 =
  Mote.Monad.test
    "Various tests for the CommitteePlainSchnorrSecp256k1ATMSPolicy token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        let
          keyCount = 80
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount
          $ Effect.Class.liftEffect
              Utils.SchnorrSecp256k1.generateRandomPrivateKey
        let
          initCommitteePubKeys = map Utils.SchnorrSecp256k1.toPubKey
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
            , initATMSKind: ATMSPlainSchnorrSecp256k1
            }

        { sidechainParams } ← initSidechain initScParams

        -- Grabbing the CommitteePlainSchnorrSecp256k1ATMSPolicy on chain parameters / minting policy
        -------------------------
        committeePlainSchnorrSecp256k1ATMSMint ←
          CommitteePlainSchnorrSecp256k1ATMSPolicy.committeePlainSchnorrSecp256k1ATMSMintFromSidechainParams
            sidechainParams

        { committeePlainSchnorrSecp256k1ATMSCurrencySymbol } ←
          CommitteePlainSchnorrSecp256k1ATMSPolicy.getCommitteePlainSchnorrSecp256k1ATMSPolicy
            committeePlainSchnorrSecp256k1ATMSMint

        -- Running the tests
        -------------------------
        logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy a successful mint from the committee"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 0s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 0

            sidechainMessage = sidechainMessageByteArray
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
            CommitteePlainSchnorrSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ←
            CommitteePlainSchnorrSecp256k1ATMSPolicy.runCommitteePlainSchnorrSecp256k1ATMSPolicy
              $ CommitteeATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeePlainSchnorrSecp256k1ATMSMint
                  , aggregateSignature: committeeSignatures
                  , message: sidechainMessageTokenName
                  }

          Test.Utils.assertIHaveOutputWithAsset
            committeePlainSchnorrSecp256k1ATMSCurrencySymbol
            sidechainMessageTokenName

        -- the following test cases are mostly duplicated code with slight
        -- variations for the testing
        logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy a successful mint from the committee with only 54/80 of the committee members"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

            sidechainMessage =
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
            CommitteePlainSchnorrSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ←
            CommitteePlainSchnorrSecp256k1ATMSPolicy.runCommitteePlainSchnorrSecp256k1ATMSPolicy
              $ CommitteeATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeePlainSchnorrSecp256k1ATMSMint
                  , aggregateSignature: committeeSignatures
                  , message: sidechainMessageTokenName
                  }

          Test.Utils.assertIHaveOutputWithAsset
            committeePlainSchnorrSecp256k1ATMSCurrencySymbol
            sidechainMessageTokenName

        logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy a successful mint from the committee where the public keys / signatures are not sorted"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

            sidechainMessage =
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
            CommitteePlainSchnorrSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ←
            CommitteePlainSchnorrSecp256k1ATMSPolicy.runCommitteePlainSchnorrSecp256k1ATMSPolicy
              $ CommitteeATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeePlainSchnorrSecp256k1ATMSMint
                  , aggregateSignature: committeeSignatures
                  , message: sidechainMessageTokenName
                  }

          Test.Utils.assertIHaveOutputWithAsset
            committeePlainSchnorrSecp256k1ATMSCurrencySymbol
            sidechainMessageTokenName

        logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy an unsuccessful mint where the committee signs all 2s, but we try to mint all 3s"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 2

            sidechainMessage =
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
            CommitteePlainSchnorrSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          void
            ( CommitteePlainSchnorrSecp256k1ATMSPolicy.runCommitteePlainSchnorrSecp256k1ATMSPolicy
                $ CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint:
                        committeePlainSchnorrSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
            )
            # Test.Utils.fails

        logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy an unsuccessful mint where we use wrong committee"
        void do
          wrongCommittee ← sequence $ Array.replicate keyCount
            $ Effect.Class.liftEffect
            $ Utils.SchnorrSecp256k1.generateRandomPrivateKey
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 4

            sidechainMessage =
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
            CommitteePlainSchnorrSecp256k1ATMSPolicy.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          void
            ( CommitteePlainSchnorrSecp256k1ATMSPolicy.runCommitteePlainSchnorrSecp256k1ATMSPolicy
                $ CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint:
                        committeePlainSchnorrSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
            )
            # Test.Utils.fails

        pure unit
