module Test.FUELMintingPolicy.V1 where

import Contract.Prelude

import Contract.Address (pubKeyHashAddress)
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
  , dummySidechainParams
  , fails
  , getOwnTransactionInput
  , plutipGroup
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELMintingPolicy.V1
  ( FuelMintParams(FuelMintParams)
  , MerkleTreeEntry(MerkleTreeEntry)
  , combinedMerkleProofToFuelParams
  , mkMintFuelLookupAndConstraints
  )
import TrustlessSidechain.Governance as Governance
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
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Tx (submitAndAwaitTx)

-- | `tests` aggregate all the FUELMintingPolicy tests in one convenient
-- | function
tests ∷ WrappedTests
tests = plutipGroup "Minting FUEL tokens using MerkleTree-based minting policy"
  $ do
      testScenarioSuccess
      testScenarioSuccess2
      testScenarioFailure
      testScenarioFailure2

-- | `testScenarioSuccess` tests minting some tokens
testScenarioSuccess ∷ PlutipTest
testScenarioSuccess = Mote.Monad.test "Claiming FUEL tokens"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 100_000_000
      , BigInt.fromInt 100_000_000
      , BigInt.fromInt 100_000_000
      , BigInt.fromInt 100_000_000
      , BigInt.fromInt 100_000_000
      ]
  $ \alice → Wallet.withKeyWallet alice do

      pkh ← getOwnPaymentPubKeyHash
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

      { sidechainParams } ← initSidechain initScParams 1
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
        $ mkMintFuelLookupAndConstraints sidechainParams
            { amount
            , recipient
            , sidechainParams
            , merkleProof
            , index
            , previousMerkleRoot
            , dsUtxo: Nothing
            }
        >>=
          submitAndAwaitTx

-- | `testScenarioSuccess2` tests minting some tokens with the fast distributed
-- | set lookup. Note: this is mostly duplicated from `testScenarioSuccess`
testScenarioSuccess2 ∷ PlutipTest
testScenarioSuccess2 =
  Mote.Monad.test "Claiming FUEL tokens with fast distributed set lookup"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice → Wallet.withKeyWallet alice do

        pkh ← getOwnPaymentPubKeyHash
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
            , initAggregatedCommittee: PlutusData.toData $ aggregateKeys
                $ map unwrap initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                pkh
            }

        { sidechainParams } ← initSidechain initScParams 1
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

          void
            $ mkMintFuelLookupAndConstraints sidechainParams
                { amount
                , recipient
                , sidechainParams
                , merkleProof
                , index
                , previousMerkleRoot
                , dsUtxo: Just nodeRef -- note that we use the distributed set UTxO in the endpoint here.
                }
            >>=
              submitAndAwaitTx

testScenarioFailure ∷ PlutipTest
testScenarioFailure =
  Mote.Monad.test "Attempt to claim with invalid merkle proof (should fail)"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 40_000_000
        ]
    $ \alice →
        Wallet.withKeyWallet alice do

          pkh ← getOwnPaymentPubKeyHash
          let
            recipient = pubKeyHashAddress pkh Nothing

          -- This is not how you create a working merkleproof that passes onchain validator.
          let mp' = unwrap $ PlutusData.serializeData (MerkleProof [])
          mt ← liftedE $ pure (fromList (pure mp'))
          mp ← liftedM "couldn't lookup merkleproof" $ pure (lookupMp mp' mt)

          void
            $ mkMintFuelLookupAndConstraints dummySidechainParams
                { merkleProof: mp
                , recipient
                , sidechainParams: dummySidechainParams
                , amount: BigInt.fromInt 1
                , index: BigInt.fromInt 0
                , previousMerkleRoot: Nothing
                , dsUtxo: Nothing
                }
            >>=
              submitAndAwaitTx
          # fails

-- | `testScenarioFailure2` tries to mint something twice (which should
-- | fail!)
testScenarioFailure2 ∷ PlutipTest
testScenarioFailure2 = Mote.Monad.test "Attempt to double claim (should fail)"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 50_000_000
      , BigInt.fromInt 50_000_000
      , BigInt.fromInt 50_000_000
      , BigInt.fromInt 40_000_000
      ]
  $ \alice →
      Wallet.withKeyWallet alice do
        -- start of mostly duplicated code from `testScenarioSuccess2`

        pkh ← getOwnPaymentPubKeyHash
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
            , initAggregatedCommittee: PlutusData.toData $ aggregateKeys
                $ map unwrap initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 2
            , initThresholdDenominator: BigInt.fromInt 3
            , initCandidatePermissionTokenMintInfo: Nothing
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap
                pkh
            }

        { sidechainParams } ← initSidechain initScParams 1

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

        FuelMintParams fp0 ← liftContractM "Could not build FuelParams" $
          combinedMerkleProofToFuelParams
            { sidechainParams
            , combinedMerkleProof: combinedMerkleProof0
            }

        -- the very bad double mint attempt...
        void $ mkMintFuelLookupAndConstraints sidechainParams fp0 >>=
          submitAndAwaitTx
        void $ mkMintFuelLookupAndConstraints sidechainParams fp0 >>=
          submitAndAwaitTx

        pure unit
        # fails
