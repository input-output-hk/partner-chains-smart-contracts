-- | `Test.CommitteePlainSchnorrSecp256k1ATMSPolicy` includes tests for the
-- | plain schnorr secp2256k1 policy.
-- | Warning: this is mostly duplicated from `Test.CommitteePlainEcdsaSecp256k1ATMSPolicy`.
module Test.CommitteePlainSchnorrSecp256k1ATMSPolicy
  ( tests
  ) where

import Contract.Prelude

import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum as BigNum
import Contract.Log (logInfo')
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.Maybe as Maybe
import Effect.Class as Effect.Class
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Partial.Unsafe as Unsafe
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils (WrappedTests, testnetGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds(ATMSPlainSchnorrSecp256k1)
  , CommitteeATMSParams(CommitteeATMSParams)
  )
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.FUEL (initFuel)
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
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
tests = testnetGroup "CommitteePlainSchnorrSecp256k1ATMSPolicy minting" $ do
  testScenario1

-- | 'testScenario1' includes various tests for `CommitteePlainSchnorrSecp256k1ATMSPolicy` from
-- | the same sidechain.
testScenario1 ∷ TestnetTest
testScenario1 =
  Mote.Monad.test
    "Various tests for the CommitteePlainSchnorrSecp256k1ATMSPolicy token"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 100_000_000
        , BigNum.fromInt 100_000_000
        , BigNum.fromInt 100_000_000
        , BigNum.fromInt 100_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        ownPaymentPubKeyHash ← getOwnPaymentPubKeyHash
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        let
          keyCount = 80
        initCommitteePrvKeys ← sequence $ Array.replicate keyCount
          $ Effect.Class.liftEffect
              Utils.SchnorrSecp256k1.generateRandomPrivateKey
        let
          initCommitteePubKeys = map Utils.SchnorrSecp256k1.toPubKey
            initCommitteePrvKeys
          aggregatedCommittee = toData
            $ unsafePartial Utils.Crypto.aggregateKeys
            $ map unwrap initCommitteePubKeys
          sidechainParams =
            SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: Governance.mkGovernanceAuthority
                  ownPaymentPubKeyHash
              }

        _ ← initTokensMint sidechainParams ATMSPlainSchnorrSecp256k1 1
        _ ←
          initFuel sidechainParams
            zero
            aggregatedCommittee
            ATMSPlainSchnorrSecp256k1
            1

        -- Grabbing the CommitteePlainSchnorrSecp256k1ATMSPolicy on chain parameters / minting policy
        -------------------------
        let
          committeePlainSchnorrSecp256k1ATMSMint =
            CommitteePlainSchnorrSecp256k1ATMSPolicy.committeePlainSchnorrSecp256k1ATMSMintFromSidechainParams
              sidechainParams

        { currencySymbol: committeePlainSchnorrSecp256k1ATMSCurrencySymbol } ←
          CommitteePlainSchnorrSecp256k1ATMSPolicy.committeePlainSchnorrSecp256k1ATMSCurrencyInfo
            { committeeCertificateMint: committeePlainSchnorrSecp256k1ATMSMint
            , sidechainParams
            }

        -- Running the tests
        -------------------------
        liftContract $ logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy a successful mint from the committee"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 0s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 0

            sidechainMessage = sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              mkAssetName sidechainMessageByteArray

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
              $
                { committeeATMSParams: CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint:
                        committeePlainSchnorrSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
                , sidechainParams
                }

          Test.Utils.assertIHaveOutputWithAsset
            ( Asset committeePlainSchnorrSecp256k1ATMSCurrencySymbol
                sidechainMessageTokenName
            )

        -- the following test cases are mostly duplicated code with slight
        -- variations for the testing
        liftContract $ logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy a successful mint from the committee with only 54/80 of the committee members"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 1s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 1

            sidechainMessage =
              sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              mkAssetName sidechainMessageByteArray

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
              $
                { committeeATMSParams: CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint:
                        committeePlainSchnorrSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
                , sidechainParams
                }

          Test.Utils.assertIHaveOutputWithAsset
            ( Asset committeePlainSchnorrSecp256k1ATMSCurrencySymbol
                sidechainMessageTokenName
            )

        liftContract $ logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy a successful mint from the committee where the public keys / signatures are not sorted"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 2s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 2

            sidechainMessage =
              sidechainMessageByteArray
            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust $
              mkAssetName sidechainMessageByteArray

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
              $
                { committeeATMSParams: CommitteeATMSParams
                    { currentCommitteeUtxo: utxo
                    , committeeCertificateMint:
                        committeePlainSchnorrSecp256k1ATMSMint
                    , aggregateSignature: committeeSignatures
                    , message: sidechainMessageTokenName
                    }
                , sidechainParams
                }

          Test.Utils.assertIHaveOutputWithAsset
            ( Asset committeePlainSchnorrSecp256k1ATMSCurrencySymbol
                sidechainMessageTokenName
            )

        liftContract $ logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy an unsuccessful mint where the committee signs all 3s, but we try to mint all 4s"
        void do
          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 3s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 3

            sidechainMessage =
              sidechainMessageByteArray

            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust
              $ mkAssetName
              $ ByteArray.byteArrayFromIntArrayUnsafe
              $ Array.replicate 32 4

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
                $
                  { committeeATMSParams: CommitteeATMSParams
                      { currentCommitteeUtxo: utxo
                      , committeeCertificateMint:
                          committeePlainSchnorrSecp256k1ATMSMint
                      , aggregateSignature: committeeSignatures
                      , message: sidechainMessageTokenName
                      }
                  , sidechainParams
                  }
            )
            # withUnliftApp Test.Utils.fails

        liftContract $ logInfo'
          "CommitteePlainSchnorrSecp256k1ATMSPolicy an unsuccessful mint where we use wrong committee"
        void do
          wrongCommittee ← sequence $ Array.replicate keyCount
            $ Effect.Class.liftEffect
            $ Utils.SchnorrSecp256k1.generateRandomPrivateKey

          let
            sidechainMessageByteArray =
              -- byte array of 32 bytes which are all 5s.
              ByteArray.byteArrayFromIntArrayUnsafe $ Array.replicate 32 5

            sidechainMessage = sidechainMessageByteArray

            sidechainMessageTokenName = Unsafe.unsafePartial $ Maybe.fromJust
              $ mkAssetName
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
                $
                  { committeeATMSParams: CommitteeATMSParams
                      { currentCommitteeUtxo: utxo
                      , committeeCertificateMint:
                          committeePlainSchnorrSecp256k1ATMSMint
                      , aggregateSignature: committeeSignatures
                      , message: sidechainMessageTokenName
                      }
                  , sidechainParams
                  }
            )
            # withUnliftApp Test.Utils.fails

        pure unit
