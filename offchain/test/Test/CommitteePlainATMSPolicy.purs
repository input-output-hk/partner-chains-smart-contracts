module Test.CommitteePlainATMSPolicy
  ( tests
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Prim.ByteArray as ByteArray
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Mote.Monad as Mote.Monad
import Partial.Unsafe as Unsafe
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteePlainATMSPolicy
  ( CommitteePlainATMSParams(CommitteePlainATMSParams)
  )
import TrustlessSidechain.CommitteePlainATMSPolicy as CommitteePlainATMSPolicy
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto
  ( SidechainMessage
  , SidechainPrivateKey
  , SidechainPublicKey
  , SidechainSignature
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

-- | `generateSignatures` generates signatures from the provided committee for
-- | the given `sidechainMessage`
generateSignatures ∷
  { -- the current committee stored on chain
    currentCommitteePrvKeys ∷ Array SidechainPrivateKey
  , sidechainMessage ∷ SidechainMessage
  } →
  Array (Tuple SidechainPublicKey SidechainSignature)
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

-- | `tests` aggregates all the `CommitteePlainATMSPolicy` tests together in
-- | one convenient function.
tests ∷ WrappedTests
tests = plutipGroup "CommitteePlainATMSPolicy minting" $ do
  testScenario1

-- | 'testScenario1' includes various tests for `CommitteePlainATMSPolicy` from
-- | the same sidechain.
testScenario1 ∷ PlutipTest
testScenario1 =
  Mote.Monad.test "Various tests for the CommitteePlainATMSPolicy token"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        let
          keyCount = 25
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount
          Utils.Crypto.generatePrivKey
        let
          initCommitteePubKeys = map Utils.Crypto.toPubKeyUnsafe
            initCommitteePrvKeys
          initScParams = InitSidechainParams
            { initChainId: BigInt.fromInt 1
            , initGenesisHash: ByteArray.hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initCommittee: initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            }

        { sidechainParams } ← initSidechain initScParams

        -- Grabbing the CommitteePlainATMSPolicy on chain parameters / minting policy
        -------------------------
        committeePlainATMSMint ←
          CommitteePlainATMSPolicy.committeePlainATMSMintFromSidechainParams
            sidechainParams

        { committeePlainATMSCurrencySymbol } ←
          CommitteePlainATMSPolicy.getCommitteePlainATMSPolicy
            committeePlainATMSMint

        -- Running the tests
        -------------------------
        logInfo' "CommitteePlainATMSPolicy a successful mint from the committee"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 0s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 0

            sidechainMessage = Utils.Crypto.byteArrayToSidechainMessageUnsafe
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
            UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ← CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy $
            CommitteePlainATMSParams
              { currentCommitteeUtxo: utxo
              , committeeCertificateMint: committeePlainATMSMint
              , signatures: committeeSignatures
              , message: sidechainMessageTokenName
              }

          Test.Utils.assertIHaveOutputWithAsset committeePlainATMSCurrencySymbol
            sidechainMessageTokenName

        -- the following test cases are mostly duplicated code with slight
        -- variations for the testing
        logInfo'
          "CommitteePlainATMSPolicy a successful mint from the committee with only 17/25 of the committee members"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

            sidechainMessage = Utils.Crypto.byteArrayToSidechainMessageUnsafe
              sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              Value.mkTokenName sidechainMessageByteArray

            allPubKeysAndSignatures = generateSignatures
              { currentCommitteePrvKeys: initCommitteePrvKeys
              , sidechainMessage: sidechainMessage
              }

            committeeSignatures =
              ( Array.take 8
                  ( map (\(pubKey /\ _sig) → pubKey /\ Nothing)
                      allPubKeysAndSignatures
                  )
              ) <>
                ( Array.drop 8
                    ( map (\(pubKey /\ sig) → pubKey /\ Just sig)
                        allPubKeysAndSignatures
                    )
                )
          -- note that we have 8 less signatures.

          utxo ←
            UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          _ ← CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy
            $ CommitteePlainATMSParams
                { currentCommitteeUtxo: utxo
                , committeeCertificateMint: committeePlainATMSMint
                , signatures: committeeSignatures
                , message: sidechainMessageTokenName
                }

          Test.Utils.assertIHaveOutputWithAsset committeePlainATMSCurrencySymbol
            sidechainMessageTokenName

        logInfo'
          "CommitteePlainATMSPolicy an unsuccessful mint where the committee signs all 2s, but we try to mint all 3s"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 2

            sidechainMessage = Utils.Crypto.byteArrayToSidechainMessageUnsafe
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
            UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          void
            ( CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy
                $ CommitteePlainATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint: committeePlainATMSMint
                    , signatures: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
            )
            # Test.Utils.fails

        logInfo'
          "CommitteePlainATMSPolicy an unsuccessful mint where we use wrong committee"
        void do
          wrongCommittee ← sequence $ Array.replicate keyCount
            Utils.Crypto.generatePrivKey
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 4

            sidechainMessage = Utils.Crypto.byteArrayToSidechainMessageUnsafe
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
            UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
              sidechainParams
          void
            ( CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy
                $ CommitteePlainATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint: committeePlainATMSMint
                    , signatures: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
            )
            # Test.Utils.fails

        pure unit
