module Test.MerkleRoot
  ( saveRoot
  , testScenario1
  , testScenario2
  , tests
  , paymentPubKeyHashToBech32Bytes
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.ToData (toData)
import Cardano.Types.Address as Address
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.NetworkId (NetworkId(TestnetId))
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash(PaymentPubKeyHash))
import Contract.Log as Log
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Wallet as Wallet
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (ByteArray)
import JS.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Run (Run)
import Run (liftEffect) as Run
import Run.Except (EXCEPT)
import Run.Except (note, rethrow) as Run
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSAggregateSignatures(PlainEcdsaSecp256k1)
  , ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.Contract (liftContract)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.FUELMintingPolicy.V1
  ( CombinedMerkleProof(CombinedMerkleProof)
  , MerkleTreeEntry(MerkleTreeEntry)
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain as InitSidechain
import TrustlessSidechain.MerkleRoot
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  )
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.MerkleTree (MerkleTree, RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  )
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1PrivateKey)
import TrustlessSidechain.Utils.Crypto as Crypto
import Type.Row (type (+))

-- | `tests` aggregates all MerkleRoot tests in a convenient single function
tests ∷ WrappedTests
tests = plutipGroup "Merkle root insertion" $ do
  testScenario1
  testScenario2
  testScenario3

-- | `paymentPubKeyHashToBech32Bytes` converts a `PaymentPubKeyHash`
-- | to the `Bech32Bytes` required for the `recipient` field of
-- | `FUELMintingPolicy.MerkleTreeEntry`.
-- | Note this assumes no staking public key hash to simplify writing tests.
paymentPubKeyHashToBech32Bytes ∷ PaymentPubKeyHash → ByteArray
paymentPubKeyHashToBech32Bytes (PaymentPubKeyHash pubKeyHash) =
  unwrap $ encodeCbor $ Address.mkPaymentAddress TestnetId
    (wrap $ PubKeyHashCredential pubKeyHash)
    Nothing

-- | `saveRoot` is a wrapper around `MerkleRoot.saveRoot` to make writing test
-- | cases a bit more terse (note that it makes all committee members sign the new root).
-- | It returns the saved merkle root.
saveRoot ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , -- merkle tree entries used to build the new merkle root
    merkleTreeEntries ∷ Array MerkleTreeEntry
  , -- the current committee's (expected to be stored on chain) private keys
    currentCommitteePrvKeys ∷ Array EcdsaSecp256k1PrivateKey
  , -- the merkle root that was just saved
    previousMerkleRoot ∷ Maybe RootHash
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + LOG + r)
    { -- merkle root that was just saved
      merkleRoot ∷ RootHash
    , -- merkle tree corresponding to the merkle root
      merkleTree ∷ MerkleTree
    , -- merkle entries and their corresponding merkle proofs
      combinedMerkleProofs ∷
        Array CombinedMerkleProof
    }
saveRoot
  { sidechainParams
  , merkleTreeEntries
  , currentCommitteePrvKeys
  , previousMerkleRoot
  } = do
  let
    serialisedEntries = map (toData >>> encodeCbor >>> unwrap)
      merkleTreeEntries
  merkleTree ← Run.rethrow <<< lmap GenericInternalError $ MerkleTree.fromArray
    serialisedEntries

  let
    merkleRoot = MerkleTree.rootHash merkleTree

  -- TODO: this has bad time complexity -- in the order of n^2.
  combinedMerkleProofs ←
    Run.note
      ( GenericInternalError
          "error 'Test.MerkleRoot.testScenario': failed to create merkle root insertion message"
      )
      $ flip traverse merkleTreeEntries
      $ \entry → do
          let serialisedEntry = unwrap $ encodeCbor $ toData entry
          merkleProof ← MerkleTree.lookupMp serialisedEntry merkleTree
          pure $ CombinedMerkleProof
            { transaction: entry
            , merkleProof
            }
  merkleRootInsertionMessage ←
    Run.note
      ( GenericInternalError
          "error 'Test.MerkleRoot.testScenario': failed to create merkle root insertion message"
      )
      $ MerkleRoot.serialiseMrimHash
      $ MerkleRootInsertionMessage
          { sidechainParams
          , merkleRoot
          , previousMerkleRoot
          }
  let
    -- make every committee member sign the new root
    committeeSignatures = Array.zip
      (map Crypto.toPubKeyUnsafe currentCommitteePrvKeys)
      ( Just <$> Crypto.multiSign currentCommitteePrvKeys
          merkleRootInsertionMessage
      )

  void $ MerkleRoot.saveRoot $ SaveRootParams
    { sidechainParams
    , merkleRoot
    , previousMerkleRoot
    , aggregateSignature: PlainEcdsaSecp256k1 committeeSignatures
    }
  pure
    { merkleRoot
    , merkleTree
    , combinedMerkleProofs
    }

-- | `testScenario1` does
-- |    1. Sets up the sidechain using the `InitSidechain.initSidechain` endpoint
-- |
-- |    2. Creates a merkle root to sign
-- |
-- |    3. Saves that merkle root with the current committee (everyone but one
-- |    person) using the `MerkleRoot.saveRoot` endpoint.
testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "Saving a Merkle root"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 40_000_000
      ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      liftContract $ Log.logInfo' "MerkleRoot testScenario1"

      -- 1. Setting up the sidechain
      ---------------------------
      let
        committeeSize = 100
      genesisUtxo ← Test.Utils.getOwnTransactionInput

      ownPaymentPubKeyHash ← getOwnPaymentPubKeyHash
      initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
        committeeSize
        Crypto.generatePrivKey
      let
        initCommitteePubKeys = map Crypto.toPubKeyUnsafe initCommitteePrvKeys
        initSidechainParams = InitSidechain.InitSidechainParams
          { initChainId: BigInt.fromInt 69
          , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
          , initUtxo: genesisUtxo
          , initAggregatedCommittee: PlutusData.toData
              $ Crypto.aggregateKeys
              $ map unwrap
                  initCommitteePubKeys
          , initSidechainEpoch: zero
          , initThresholdNumerator: BigInt.fromInt 2
          , initThresholdDenominator: BigInt.fromInt 3
          , initCandidatePermissionTokenMintInfo: Nothing
          , initATMSKind: ATMSPlainEcdsaSecp256k1
          , initGovernanceAuthority: Governance.mkGovernanceAuthority
              ownPaymentPubKeyHash
          }

      { sidechainParams } ← InitSidechain.initSidechain initSidechainParams 1

      -- Building / saving the root that pays lots of FUEL to this wallet :)
      ----------------------------------------------------------------------
      let ownRecipient = paymentPubKeyHashToBech32Bytes ownPaymentPubKeyHash
      let
        serialisedEntries = map (toData >>> encodeCbor >>> unwrap) $
          [ MerkleTreeEntry
              { index: BigInt.fromInt 0
              , amount: BigInt.fromInt 69
              , previousMerkleRoot: Nothing
              , recipient: ownRecipient
              }
          ]
      merkleTree ← Run.rethrow <<< lmap GenericInternalError $
        MerkleTree.fromArray serialisedEntries

      let
        merkleRoot = MerkleTree.rootHash merkleTree

      merkleRootInsertionMessage ←
        Run.note
          ( GenericInternalError
              "error 'Test.MerkleRoot.testScenario1': failed to create merkle root insertion message"
          )
          $ MerkleRoot.serialiseMrimHash
          $ MerkleRootInsertionMessage
              { sidechainParams: sidechainParams
              , merkleRoot
              , previousMerkleRoot: Nothing
              }
      let
        -- We create signatures for every committee member BUT the first key...
        -- if you wanted to create keys for every committee member, we would write
        -- ```
        -- committeeSignatures = Array.zip
        --     initCommitteePubKeys
        --     (Just <$> Crypto.multiSign initCommitteePrvKeys merkleRootInsertionMessage)
        -- ```
        committeeSignatures =
          case
            Array.uncons $ Array.zip
              initCommitteePubKeys
              ( Just <$> Crypto.multiSign initCommitteePrvKeys
                  merkleRootInsertionMessage
              )
            of
            Just { head, tail } →
              Array.cons ((fst head) /\ Nothing) tail
            _ → [] -- should never happen

      void $ MerkleRoot.saveRoot $ SaveRootParams
        { sidechainParams
        , merkleRoot
        , previousMerkleRoot: Nothing

        , aggregateSignature: PlainEcdsaSecp256k1
            committeeSignatures
        }

      pure unit

-- | `testScenario2` does the following
-- |    1. initializes the sidechain
-- |
-- |    2. saves a merkle root
-- |
-- |    3. saves another merkle root (this references the last merkle root).
-- |
-- | Note: the initialize sidechain part is duplicated code from above.
testScenario2 ∷ PlutipTest
testScenario2 = Mote.Monad.test "Saving two merkle roots"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 50_000_000
      , BigNum.fromInt 40_000_000
      ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      liftContract $ Log.logInfo' "MerkleRoot testScenario2"

      -- 1. Setting up the sidechain
      ---------------------------
      let
        committeeSize = 25
      -- It fails with ~50 (nondeterministically) with budget overspent
      -- I would really like to get this up to 101 as with the update
      -- committee hash endpoint! Some room for optimization is certainly
      -- a possibility..
      genesisUtxo ← Test.Utils.getOwnTransactionInput

      ownPaymentPubKeyHash ← getOwnPaymentPubKeyHash
      initCommitteePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
        committeeSize
        Crypto.generatePrivKey
      let
        initCommitteePubKeys = map Crypto.toPubKeyUnsafe initCommitteePrvKeys
        initSidechainParams = InitSidechain.InitSidechainParams
          { initChainId: BigInt.fromInt 69
          , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
          , initUtxo: genesisUtxo
          , initAggregatedCommittee: PlutusData.toData
              $ Crypto.aggregateKeys
              $ map unwrap
                  initCommitteePubKeys
          , initSidechainEpoch: zero
          , initThresholdNumerator: BigInt.fromInt 2
          , initThresholdDenominator: BigInt.fromInt 3
          , initCandidatePermissionTokenMintInfo: Nothing
          , initATMSKind: ATMSPlainEcdsaSecp256k1
          , initGovernanceAuthority: Governance.mkGovernanceAuthority
              ownPaymentPubKeyHash
          }

      { sidechainParams } ← InitSidechain.initSidechain initSidechainParams 1

      -- Building / saving the root that pays lots of FUEL to this wallet :)
      ----------------------------------------------------------------------
      let ownRecipient = paymentPubKeyHashToBech32Bytes ownPaymentPubKeyHash

      { merkleRoot: merkleRoot1 } ←
        saveRoot
          { sidechainParams
          , merkleTreeEntries:
              [ MerkleTreeEntry
                  { index: BigInt.fromInt 0
                  , amount: BigInt.fromInt 69
                  , previousMerkleRoot: Nothing
                  , recipient: ownRecipient
                  }
              ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }
      _ ←
        saveRoot
          { sidechainParams
          , merkleTreeEntries:
              [ MerkleTreeEntry
                  { index: BigInt.fromInt 0
                  , amount: BigInt.fromInt 69
                  , previousMerkleRoot: Just merkleRoot1
                  , recipient: ownRecipient
                  }
              , MerkleTreeEntry
                  { index: BigInt.fromInt 1
                  , amount: BigInt.fromInt 69
                  , previousMerkleRoot: Just merkleRoot1
                  , recipient: ownRecipient
                  }
              ]
          , currentCommitteePrvKeys: initCommitteePrvKeys
          , previousMerkleRoot: Nothing
          }
      pure unit

-- | `testScenario3` does the following
-- |    1. initializes the sidechain with repeated committee members
-- |
-- |    2. saves a merkle root
-- |
-- | Note: there is significant duplicated code from `testScenario2`
testScenario3 ∷ PlutipTest
testScenario3 =
  Mote.Monad.test "Saving a merkle root with a largely duplicated committee"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        , BigNum.fromInt 50_000_000
        ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        liftContract $ Log.logInfo' "MerkleRoot testScenario2"

        -- 1. Setting up the sidechain
        ---------------------------
        let
          committeeSize = 70
        genesisUtxo ← Test.Utils.getOwnTransactionInput
        pkh ← getOwnPaymentPubKeyHash

        -- Create two distinguished guys that we'll duplicate 5 and 15 times resp.
        duplicated1PrvKey ← Run.liftEffect $ Crypto.generatePrivKey
        duplicated2PrvKey ← Run.liftEffect $ Crypto.generatePrivKey

        everyoneElsePrvKeys ← Run.liftEffect $ sequence $ Array.replicate
          (committeeSize - 20)
          Crypto.generatePrivKey
        let
          initCommitteePrvKeys = Array.replicate 5 duplicated1PrvKey
            <> everyoneElsePrvKeys
            <> Array.replicate 15 duplicated2PrvKey
          initCommitteePubKeys = map Crypto.toPubKeyUnsafe initCommitteePrvKeys
          initSidechainParams = InitSidechain.InitSidechainParams
            { initChainId: BigInt.fromInt 69
            , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
            , initUtxo: genesisUtxo
            , initAggregatedCommittee: PlutusData.toData
                $ Crypto.aggregateKeys
                $ map unwrap
                    initCommitteePubKeys
            , initSidechainEpoch: zero
            , initThresholdNumerator: BigInt.fromInt 99999
            , initThresholdDenominator: BigInt.fromInt 100000
            , initCandidatePermissionTokenMintInfo: Nothing
            , initATMSKind: ATMSPlainEcdsaSecp256k1
            , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
            }

        { sidechainParams } ← InitSidechain.initSidechain initSidechainParams 1

        -- Building / saving the root that pays lots of FUEL to this wallet :)
        ----------------------------------------------------------------------

        let ownRecipient = paymentPubKeyHashToBech32Bytes pkh

        { merkleRoot: merkleRoot1 } ←
          saveRoot
            { sidechainParams
            , merkleTreeEntries:
                [ MerkleTreeEntry
                    { index: BigInt.fromInt 0
                    , amount: BigInt.fromInt 69
                    , previousMerkleRoot: Nothing
                    , recipient: ownRecipient
                    }
                ]
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , previousMerkleRoot: Nothing
            }
        _ ←
          saveRoot
            { sidechainParams
            , merkleTreeEntries:
                [ MerkleTreeEntry
                    { index: BigInt.fromInt 0
                    , amount: BigInt.fromInt 69
                    , previousMerkleRoot: Just merkleRoot1
                    , recipient: ownRecipient
                    }
                , MerkleTreeEntry
                    { index: BigInt.fromInt 1
                    , amount: BigInt.fromInt 69
                    , previousMerkleRoot: Just merkleRoot1
                    , recipient: ownRecipient
                    }
                ]
            , currentCommitteePrvKeys: initCommitteePrvKeys
            , previousMerkleRoot: Nothing
            }
        pure unit
