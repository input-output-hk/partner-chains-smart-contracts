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
  ( ATMSPlainMultisignature(ATMSPlainMultisignature)
  , CommitteePlainATMSParams(CommitteePlainATMSParams)
  )
import TrustlessSidechain.CommitteePlainATMSPolicy as CommitteePlainATMSPolicy
import TrustlessSidechain.InitSidechain (InitSidechainParams(..), initSidechain)
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

-- | `tests` aggregates all UpdateCommitteeHash the tests.
tests ∷ WrappedTests
tests = plutipGroup "Signed committee token minting" $ do
  testScenario1

-- | 'testScenario1' updates the committee hash
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "Various tests for the committee signed token"
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

      -- Grabbing the committee signed token on chain parameters / minting policy
      -------------------------
      committeeSignedTokenMint ←
        CommitteePlainATMSPolicy.committeeSignedTokenMintFromSidechainParams
          sidechainParams

      { committeeSignedTokenCurrencySymbol } ←
        CommitteePlainATMSPolicy.getCommitteePlainATMSPolicy
          committeeSignedTokenMint

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

          committeeSignedTokenRedeemer = ATMSPlainMultisignature
            { currentCommittee: map fst committeeSignatures
            , currentCommitteeSignatures: map snd committeeSignatures
            }

          committeeSignatures = generateSignatures
            { -- the current committee stored on chain
              currentCommitteePrvKeys: initCommitteePrvKeys
            , sidechainMessage: sidechainMessage
            }

        -- TODO: CTL updates removed the required functions for `assertMaxFee`,
        -- so this function no longer exists... but perhaps one day we should
        -- get this back. Boo hoo!
        -- ```
        -- Test.Utils.assertMaxFee (BigInt.fromInt 1_000_000)
        -- ```

        utxo ← UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
          sidechainParams
        _ ← CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy $
          CommitteePlainATMSParams
            { currentCommitteeUtxo: utxo
            , committeeCertificateMint: committeeSignedTokenMint
            , atmsPlainMultisignature: committeeSignedTokenRedeemer
            , message: sidechainMessageTokenName
            }

        Test.Utils.assertIHaveOutputWithAsset committeeSignedTokenCurrencySymbol
          sidechainMessageTokenName

      -- the following test cases are mostly duplicated code with slight
      -- variations for the testing
      logInfo'
        "CommitteePlainATMSPolicy a successful mint from the committee with only 20/25 of the committee members"
      void do
        let
          sidechainMessageByteArray =
            -- byte array of 32 bytes which are all 1s.
            ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

          sidechainMessage = Utils.Crypto.byteArrayToSidechainMessageUnsafe
            sidechainMessageByteArray
          sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
            Value.mkTokenName sidechainMessageByteArray

          committeeSignedTokenRedeemer = ATMSPlainMultisignature
            { currentCommittee: map fst committeeSignatures
            , currentCommitteeSignatures: Array.drop 5 $ map snd
                committeeSignatures
            }

          committeeSignatures = generateSignatures
            { -- the current committee stored on chain
              currentCommitteePrvKeys: initCommitteePrvKeys
            , sidechainMessage: sidechainMessage
            }

        utxo ← UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
          sidechainParams
        _ ← CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy
          $ CommitteePlainATMSParams
              { currentCommitteeUtxo: utxo
              , committeeCertificateMint: committeeSignedTokenMint
              , atmsPlainMultisignature: committeeSignedTokenRedeemer
              , message: sidechainMessageTokenName
              }

        Test.Utils.assertIHaveOutputWithAsset committeeSignedTokenCurrencySymbol
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

          committeeSignedTokenRedeemer = ATMSPlainMultisignature
            { currentCommittee: map fst committeeSignatures
            , currentCommitteeSignatures: map snd committeeSignatures
            }

          committeeSignatures = generateSignatures
            { -- the current committee stored on chain
              currentCommitteePrvKeys: initCommitteePrvKeys
            , sidechainMessage
            }

        utxo ← UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
          sidechainParams
        void
          ( CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy
              $ CommitteePlainATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeeSignedTokenMint
                  , atmsPlainMultisignature: committeeSignedTokenRedeemer
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

          committeeSignedTokenRedeemer = ATMSPlainMultisignature
            { currentCommittee: map fst committeeSignatures
            , currentCommitteeSignatures: map snd committeeSignatures
            }

          committeeSignatures = generateSignatures
            { -- the current committee stored on chain
              currentCommitteePrvKeys: wrongCommittee
            , sidechainMessage: sidechainMessage
            }

        utxo ← UpdateCommitteeHash.findUpdateCommitteeHashUtxoFromSidechainParams
          sidechainParams
        void
          ( CommitteePlainATMSPolicy.runCommitteePlainATMSPolicy
              $ CommitteePlainATMSParams
                  { currentCommitteeUtxo: utxo
                  , committeeCertificateMint: committeeSignedTokenMint
                  , atmsPlainMultisignature: committeeSignedTokenRedeemer
                  , message: sidechainMessageTokenName
                  }
          )
          # Test.Utils.fails

      pure unit
