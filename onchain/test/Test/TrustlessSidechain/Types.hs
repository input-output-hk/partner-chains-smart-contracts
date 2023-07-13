{-# LANGUAGE DuplicateRecordFields #-}

module Test.TrustlessSidechain.Types where

import TrustlessSidechain.HaskellPrelude

import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import Data.String qualified as HString
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Ledger.Crypto (Signature (Signature))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Tx (TxId (TxId), TxOutRef (TxOutRef))
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import TrustlessSidechain.MerkleTree (MerkleProof)
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.Types (
  BlockProducerRegistration (
    BlockProducerRegistration,
    bprInputUtxo,
    bprOwnPkh,
    bprSidechainPubKey,
    bprSidechainSignature,
    bprSpoPubKey,
    bprSpoSignature
  ),
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmInputUtxo,
    bprmSidechainParams,
    bprmSidechainPubKey
  ),
  CandidatePermissionMint (CandidatePermissionMint, cpmSidechainParams, cpmUtxo),
  CheckpointDatum (
    CheckpointDatum,
    checkpointBlockHash,
    checkpointBlockNumber
  ),
  CheckpointMessage (
    CheckpointMessage,
    checkpointMsgBlockHash,
    checkpointMsgBlockNumber,
    checkpointMsgSidechainEpoch,
    checkpointMsgSidechainParams
  ),
  CheckpointParameter (
    CheckpointParameter,
    checkpointAssetClass,
    checkpointSidechainParams,
    committeeHashAssetClass
  ),
  CheckpointRedeemer (
    CheckpointRedeemer,
    checkpointCommitteePubKeys,
    checkpointCommitteeSignatures,
    newCheckpointBlockHash,
    newCheckpointBlockNumber
  ),
  CombinedMerkleProof (
    CombinedMerkleProof,
    cmpMerkleProof,
    cmpTransaction
  ),
  FUELMint (
    FUELMint,
    fmDsKeyCurrencySymbol,
    fmMptRootTokenCurrencySymbol,
    fmSidechainParams
  ),
  FUELRedeemer (MainToSide, SideToMain),
  GenesisHash (GenesisHash),
  MerkleRootInsertionMessage (
    MerkleRootInsertionMessage,
    mrimMerkleRoot,
    mrimPreviousMerkleRoot,
    mrimSidechainParams
  ),
  MerkleTreeEntry (
    MerkleTreeEntry,
    mteAmount,
    mteIndex,
    mtePreviousMerkleRoot,
    mteRecipient
  ),
  SidechainParams (
    SidechainParams,
    chainId,
    genesisHash,
    genesisUtxo,
    thresholdDenominator,
    thresholdNumerator
  ),
  SidechainPubKey (SidechainPubKey),
  SignedMerkleRoot (
    SignedMerkleRoot,
    committeePubKeys,
    merkleRoot,
    previousMerkleRoot,
    signatures
  ),
  SignedMerkleRootMint (
    SignedMerkleRootMint,
    smrmSidechainParams,
    smrmUpdateCommitteeHashCurrencySymbol,
    smrmValidatorHash
  ),
  UpdateCommitteeHash (
    UpdateCommitteeHash,
    cMptRootTokenCurrencySymbol,
    cSidechainParams,
    cToken
  ),
  UpdateCommitteeHashDatum (
    UpdateCommitteeHashDatum,
    committeeHash,
    sidechainEpoch
  ),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    uchmNewCommitteePubKeys,
    uchmPreviousMerkleRoot,
    uchmSidechainEpoch,
    uchmSidechainParams
  ),
  UpdateCommitteeHashRedeemer (
    UpdateCommitteeHashRedeemer,
    committeePubKeys,
    committeeSignatures,
    newCommitteePubKeys,
    previousMerkleRoot
  ),
 )

tests :: TestTree
tests =
  testGroup
    "Golden tests of ToData encoded data"
    [ dataEncoderGoldenTest "SidechainParams" dummySidechainParams
    , dataEncoderGoldenTest "CandidatePermissionMint" dummyCandidatePermissionMint
    , dataEncoderGoldenTest "BlockProducerRegistration" dummyBlockProducerRegistration
    , dataEncoderGoldenTest "BlockProducerRegistrationMsg" dummyBlockProducerRegistrationMsg
    , dataEncoderGoldenTest "MerkleTreeEntry" dummyMerkleTreeEntry
    , dataEncoderGoldenTest "MerkleRootInsertionMessage" dummyMerkleRootInsertionMessage
    , dataEncoderGoldenTest "SignedMerkleRoot" dummySignedMerkleRoot
    , dataEncoderGoldenTest "SignedMerkleRootMint" dummySignedMerkleRootMint
    , dataEncoderGoldenTest "CombinedMerkleProof" dummyCombinedMerkleProof
    , dataEncoderGoldenTest "FUELReedemer1" dummyFUELReedemer1
    , dataEncoderGoldenTest "FUELReedemer2" dummyFUELReedemer2
    , dataEncoderGoldenTest "FUELMint" dummyFUELMint
    , dataEncoderGoldenTest "UpdateCommitteeHashDatum" dummyUpdateCommitteeHashDatum
    , dataEncoderGoldenTest "UpdateCommitteeHashRedeemer" dummyUpdateCommitteeHashRedeemer
    , dataEncoderGoldenTest "UpdateCommitteeHash" dummyUpdateCommitteeHash
    , dataEncoderGoldenTest "UpdateCommitteeHashMessage" dummyUpdateCommitteeHashMessage
    , dataEncoderGoldenTest "CheckpointDatum" dummyCheckpointDatum
    , dataEncoderGoldenTest "CheckpointRedeemer" dummyCheckpointRedeemer
    , dataEncoderGoldenTest "CheckpointParameter" dummyCheckpointParameter
    , dataEncoderGoldenTest "CheckpointMessage" dummyCheckpointMessage
    ]

dummyTxOutRef :: TxOutRef
dummyTxOutRef = TxOutRef (TxId "e41c9b57841e582c207bb68d5e9736fb48c7af5f1ec29ade00692fa5e0e47efa") 4

dummySidechainParams :: SidechainParams
dummySidechainParams =
  SidechainParams
    { chainId = 11
    , genesisHash = GenesisHash "aabbcc"
    , genesisUtxo = dummyTxOutRef
    , thresholdNumerator = 2
    , thresholdDenominator = 3
    }

dummyCandidatePermissionMint :: CandidatePermissionMint
dummyCandidatePermissionMint =
  CandidatePermissionMint
    { cpmSidechainParams = dummySidechainParams
    , cpmUtxo = dummyTxOutRef
    }

dummyBlockProducerRegistration :: BlockProducerRegistration
dummyBlockProducerRegistration =
  BlockProducerRegistration
    { bprSpoPubKey = "aabbcc"
    , bprSidechainPubKey = SidechainPubKey "bbccdd"
    , bprSpoSignature = Signature "ccddee"
    , bprSidechainSignature = Signature "ddeeff"
    , bprInputUtxo = dummyTxOutRef
    , bprOwnPkh = "eeff00"
    }

dummyBlockProducerRegistrationMsg :: BlockProducerRegistrationMsg
dummyBlockProducerRegistrationMsg =
  BlockProducerRegistrationMsg
    { bprmSidechainParams = dummySidechainParams
    , bprmSidechainPubKey = SidechainPubKey "bbccdd"
    , bprmInputUtxo = dummyTxOutRef
    }

dummyMerkleTreeEntry :: MerkleTreeEntry
dummyMerkleTreeEntry =
  MerkleTreeEntry
    { mteIndex = 5
    , mteAmount = 100
    , mteRecipient = "0xaabbcc"
    , mtePreviousMerkleRoot = Just "aabbcc"
    }

dummyMerkleRootInsertionMessage :: MerkleRootInsertionMessage
dummyMerkleRootInsertionMessage =
  MerkleRootInsertionMessage
    { mrimSidechainParams = dummySidechainParams
    , mrimMerkleRoot = "aabbcc"
    , mrimPreviousMerkleRoot = Just "bbccdd"
    }

dummySignedMerkleRoot :: SignedMerkleRoot
dummySignedMerkleRoot =
  SignedMerkleRoot
    { merkleRoot = "aabbcc"
    , previousMerkleRoot = Just "bbccdd"
    , signatures = ["a01122", "b01122"]
    , committeePubKeys = [SidechainPubKey "a11122", SidechainPubKey "b11122"]
    }

dummySignedMerkleRootMint :: SignedMerkleRootMint
dummySignedMerkleRootMint =
  SignedMerkleRootMint
    { smrmSidechainParams = dummySidechainParams
    , smrmUpdateCommitteeHashCurrencySymbol = "001122"
    , smrmValidatorHash = "aabbcc"
    }

dummyMerkleProof :: MerkleProof
dummyMerkleProof =
  let merkleTree = MerkleTree.fromList ["aabbcc", "bbccdd", "ccddee", "ddeeff"]
   in fromJust $ MerkleTree.lookupMp "ddeeff" merkleTree

dummyCombinedMerkleProof :: CombinedMerkleProof
dummyCombinedMerkleProof =
  CombinedMerkleProof
    { cmpTransaction = dummyMerkleTreeEntry
    , cmpMerkleProof = dummyMerkleProof
    }

dummyFUELReedemer1 :: FUELRedeemer
dummyFUELReedemer1 =
  MainToSide "aabbcc"

dummyFUELReedemer2 :: FUELRedeemer
dummyFUELReedemer2 =
  SideToMain dummyMerkleTreeEntry dummyMerkleProof

dummyFUELMint :: FUELMint
dummyFUELMint =
  FUELMint
    { fmMptRootTokenCurrencySymbol = "aabbcc"
    , fmSidechainParams = dummySidechainParams
    , fmDsKeyCurrencySymbol = "ccddee"
    }

dummyUpdateCommitteeHashDatum :: UpdateCommitteeHashDatum
dummyUpdateCommitteeHashDatum =
  UpdateCommitteeHashDatum
    { committeeHash = "aabbcc"
    , sidechainEpoch = 48
    }

dummyUpdateCommitteeHashRedeemer :: UpdateCommitteeHashRedeemer
dummyUpdateCommitteeHashRedeemer =
  UpdateCommitteeHashRedeemer
    { committeeSignatures = ["aabbcc", "bbccdd"]
    , committeePubKeys = [SidechainPubKey "ccddee", SidechainPubKey "eeff00"]
    , newCommitteePubKeys = [SidechainPubKey "001122", SidechainPubKey "112233"]
    , previousMerkleRoot = Just "bbccdd"
    }

dummyUpdateCommitteeHash :: UpdateCommitteeHash
dummyUpdateCommitteeHash =
  UpdateCommitteeHash
    { cSidechainParams = dummySidechainParams
    , cToken = Value.assetClass "aabbcc" "ccddee"
    , cMptRootTokenCurrencySymbol = "ddee"
    }

dummyUpdateCommitteeHashMessage :: UpdateCommitteeHashMessage
dummyUpdateCommitteeHashMessage =
  UpdateCommitteeHashMessage
    { uchmSidechainParams = dummySidechainParams
    , uchmNewCommitteePubKeys = [SidechainPubKey "aabbcc", SidechainPubKey "bbccdd"]
    , uchmPreviousMerkleRoot = Just "bbccdd"
    , uchmSidechainEpoch = 14
    }

dummyCheckpointDatum :: CheckpointDatum
dummyCheckpointDatum =
  CheckpointDatum
    { checkpointBlockHash = "aabbcc"
    , checkpointBlockNumber = 14
    }

dummyCheckpointRedeemer :: CheckpointRedeemer
dummyCheckpointRedeemer =
  CheckpointRedeemer
    { checkpointCommitteeSignatures = ["aabbcc", "bbccdd"]
    , checkpointCommitteePubKeys = [SidechainPubKey "aabbcc", SidechainPubKey "bbccdd"]
    , newCheckpointBlockHash = "00123456"
    , newCheckpointBlockNumber = 15
    }

dummyCheckpointParameter :: CheckpointParameter
dummyCheckpointParameter =
  CheckpointParameter
    { checkpointSidechainParams = dummySidechainParams
    , checkpointAssetClass = Value.assetClass "aabbcc" "112233"
    , committeeHashAssetClass = Value.assetClass "bbccdd" "001122"
    }

dummyCheckpointMessage :: CheckpointMessage
dummyCheckpointMessage =
  CheckpointMessage
    { checkpointMsgSidechainParams = dummySidechainParams
    , checkpointMsgBlockHash = "00123456"
    , checkpointMsgBlockNumber = 15
    , checkpointMsgSidechainEpoch = 16
    }

dataEncoderGoldenTest :: ToData a => HString.String -> a -> TestTree
dataEncoderGoldenTest name =
  goldenVsString ("IsData encoded " <> name) ("./test/golden/" <> name <> ".golden") . encode
  where
    encode = pure . fromStrict . encodeUtf8 . Text.pack . show . toBuiltinData
