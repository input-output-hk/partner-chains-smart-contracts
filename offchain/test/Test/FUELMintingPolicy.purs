module Test.FUELMintingPolicy (tests) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash, pubKeyHashAddress)
import Contract.Hashing (blake2b256Hash)
import Contract.Monad (liftContractM, liftedE, liftedM)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Test.MerkleRoot as Test.MerkleRoot
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils
  ( WrappedTests
  , fails
  , getOwnTransactionInput
  , plutipGroup
  , toTxIn
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELMintingPolicy
  ( FuelParams(Mint, Burn)
  , MerkleTreeEntry(MerkleTreeEntry)
  , combinedMerkleProofToFuelParams
  , runFuelMP
  )
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.MerkleTree
  ( MerkleProof(MerkleProof)
  , fromList
  , lookupMp
  )
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Crypto (generatePrivKey, toPubKeyUnsafe)

-- | `tests` aggregate all the FUELMintingPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = plutipGroup "Claiming and burning FUEL tokens" $ do
  testScenarioSuccess
  testScenarioSuccess2
  testScenarioSuccess3
  testScenarioFailure
  testScenarioFailure2

-- | `testScenarioSuccess` tests minting some tokens
testScenarioSuccess ∷ PlutipTest
testScenarioSuccess = Mote.Monad.test "Claiming FUEL tokens"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
      ownRecipient ←
        liftContractM "Could not convert pub key hash to bech 32 bytes" $
          Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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
          , initCommittee: initCommitteePubKeys
          , initSidechainEpoch: zero
          , initThresholdNumerator: BigInt.fromInt 2
          , initThresholdDenominator: BigInt.fromInt 3
          , initCandidatePermissionTokenMintInfo: Nothing
          }

      { sidechainParams } ← initSidechain initScParams
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

      void $ runFuelMP sidechainParams
        ( Mint
            { amount
            , recipient
            , sidechainParams
            , merkleProof
            , index
            , previousMerkleRoot
            , dsUtxo: Nothing
            }
        )

-- | `testScenarioSuccess2` mints and burns a few times.. In particular, we:
-- |    - mint 5
-- |    - mint 7
-- |    - burn 10
-- |    - burn 2
testScenarioSuccess2 ∷ PlutipTest
testScenarioSuccess2 =
  Mote.Monad.test
    "Multiple claim and burn steps (minting 5 FUEL, minting 7 FUEL, burning 10 FUEL, burning 2 FUEL)"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000
        , BigInt.fromInt 10_000_000
        , BigInt.fromInt 10_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do
        -- start of mostly duplicated code from `testScenarioSuccess`
        pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
        ownRecipient ←
          liftContractM "Could not convert pub key hash to bech 32 bytes" $
            Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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
            , initCommittee: initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            }
        -- end of mostly duplicated code from `testScenarioSuccess`

        { sidechainParams } ← initSidechain initScParams

        { combinedMerkleProofs } ← Test.MerkleRoot.saveRoot
          { sidechainParams
          , merkleTreeEntries:
              let
                previousMerkleRoot = Nothing
                entry0 =
                  MerkleTreeEntry
                    { index: BigInt.fromInt 0
                    , amount: BigInt.fromInt 5
                    , previousMerkleRoot
                    , recipient: ownRecipient
                    }
                entry1 =
                  MerkleTreeEntry
                    { index: BigInt.fromInt 1
                    , amount: BigInt.fromInt 7
                    , previousMerkleRoot
                    , recipient: ownRecipient
                    }
              in
                [ entry0, entry1 ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }

        (combinedMerkleProof0 /\ combinedMerkleProof1) ←
          liftContractM "bad test case for `testScenarioSuccess2`"
            $ case combinedMerkleProofs of
                [ combinedMerkleProof0, combinedMerkleProof1 ] → pure
                  $ combinedMerkleProof0
                  /\ combinedMerkleProof1
                _ → Nothing

        fp0 ← liftContractM "Could not build FuelParams" $
          combinedMerkleProofToFuelParams sidechainParams combinedMerkleProof0

        fp1 ← liftContractM "Could not build FuelParams" $
          combinedMerkleProofToFuelParams sidechainParams combinedMerkleProof1

        -- TODO: see definition of assertMaxFee
        -- assertMaxFee (BigInt.fromInt 1_350_000) =<<
        void $ runFuelMP sidechainParams fp0
        -- assertMaxFee (BigInt.fromInt 1_350_000) =<<
        void $ runFuelMP sidechainParams fp1

        -- assertMaxFee (BigInt.fromInt 500_000) =<<
        void $ runFuelMP sidechainParams
          ( Burn
              { amount: BigInt.fromInt 10
              , recipient: hexToByteArrayUnsafe "aabbcc"
              }
          )

        -- assertMaxFee (BigInt.fromInt 500_000) =<<
        void $ runFuelMP sidechainParams
          ( Burn
              { amount: BigInt.fromInt 2
              , recipient: hexToByteArrayUnsafe "aabbcc"
              }
          )

        pure unit

-- | `testScenarioSuccess3` tests minting some tokens with the fast distributed
-- | set lookup. Note: this is mostly duplicated from `testScenarioSuccess`
testScenarioSuccess3 ∷ PlutipTest
testScenarioSuccess3 =
  Mote.Monad.test "Claiming FUEL tokens with fast distributed set lookup"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
        ownRecipient ←
          liftContractM "Could not convert pub key hash to bech 32 bytes" $
            Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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
            , initCommittee: initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            }

        { sidechainParams } ← initSidechain initScParams
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
          ownEntryHash = blake2b256Hash ownEntryBytes

          ownEntryHashTn = unsafePartial $ fromJust $ Value.mkTokenName
            ownEntryHash
          merkleTree =
            unsafePartial $ fromJust $ hush $ MerkleTree.fromArray
              [ ownEntryBytes ]

          merkleProof = unsafePartial $ fromJust $ MerkleTree.lookupMp
            ownEntryBytes
            merkleTree

        void $ Test.MerkleRoot.saveRoot
          { sidechainParams
          , merkleTreeEntries: [ ownEntry ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }

        void do
          ds ← DistributedSet.getDs (unwrap sidechainParams).genesisUtxo

          -- we first grab the distributed set UTxO (the slow way as we have no
          -- other mechanism for doing this with ctl)
          { inUtxo: { nodeRef } } ← liftedM "error no distributed set node found"
            $ DistributedSet.slowFindDsOutput ds ownEntryHashTn

          void $ runFuelMP sidechainParams
            ( Mint
                { amount
                , recipient
                , sidechainParams
                , merkleProof
                , index
                , previousMerkleRoot
                , dsUtxo: Just nodeRef -- note that we use the distributed set UTxO in the endpoint here.
                }
            )

testScenarioFailure ∷ PlutipTest
testScenarioFailure =
  Mote.Monad.test "Attempt to claim with invalid merkle proof (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice →
        Wallet.withKeyWallet alice do
          pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
          let
            recipient = pubKeyHashAddress pkh Nothing
            scParams = SidechainParams
              { chainId: BigInt.fromInt 1
              , genesisHash: hexToByteArrayUnsafe "aabbcc"
              , genesisUtxo: toTxIn "aabbcc" 0
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              }

          -- This is not how you create a working merkleproof that passes onchain validator.
          let mp' = unwrap $ PlutusData.serializeData (MerkleProof [])
          mt ← liftedE $ pure (fromList (pure mp'))
          mp ← liftedM "couldn't lookup merkleproof" $ pure (lookupMp mp' mt)

          void $ runFuelMP scParams $ Mint
            { merkleProof: mp
            , recipient
            , sidechainParams: scParams
            , amount: BigInt.fromInt 1
            , index: BigInt.fromInt 0
            , previousMerkleRoot: Nothing
            , dsUtxo: Nothing
            }
          void $ runFuelMP scParams $ Burn
            { amount: BigInt.fromInt 1, recipient: hexToByteArrayUnsafe "aabbcc" }
          # fails

-- | `testScenarioFailure2` tries to mint something twice (which should
-- | fail!)
testScenarioFailure2 ∷ PlutipTest
testScenarioFailure2 = Mote.Monad.test "Attempt to double claim (should fail)"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice →
      Wallet.withKeyWallet alice do
        -- start of mostly duplicated code from `testScenarioSuccess2`
        pkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
        ownRecipient ←
          liftContractM "Could not convert pub key hash to bech 32 bytes" $
            Test.MerkleRoot.paymentPubKeyHashToBech32Bytes pkh
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
            , initCommittee: initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            }

        { sidechainParams } ← initSidechain initScParams

        { combinedMerkleProofs } ← Test.MerkleRoot.saveRoot
          { sidechainParams
          , merkleTreeEntries:
              let
                previousMerkleRoot = Nothing
                entry0 =
                  MerkleTreeEntry
                    { index: BigInt.fromInt 0
                    , amount: BigInt.fromInt 5
                    , previousMerkleRoot
                    , recipient: ownRecipient
                    }
                entry1 =
                  MerkleTreeEntry
                    { index: BigInt.fromInt 1
                    , amount: BigInt.fromInt 7
                    , previousMerkleRoot
                    , recipient: ownRecipient
                    }
              in
                [ entry0, entry1 ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }
        -- end of mostly duplicated code from `testScenarioSuccess2`

        (combinedMerkleProof0 /\ _combinedMerkleProof1) ←
          liftContractM "bad test case for `testScenarioSuccess2`"
            $ case combinedMerkleProofs of
                [ combinedMerkleProof0, combinedMerkleProof1 ] → pure
                  $ combinedMerkleProof0
                  /\ combinedMerkleProof1
                _ → Nothing

        fp0 ← liftContractM "Could not build FuelParams" $
          combinedMerkleProofToFuelParams sidechainParams combinedMerkleProof0

        -- the very bad double mint attempt...
        void $ runFuelMP sidechainParams fp0
        void $ runFuelMP sidechainParams fp0

        pure unit
        # fails
