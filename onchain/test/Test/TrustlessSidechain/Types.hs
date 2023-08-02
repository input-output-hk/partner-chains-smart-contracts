{-# LANGUAGE DuplicateRecordFields #-}

module Test.TrustlessSidechain.Types (tests) where

import TrustlessSidechain.HaskellPrelude

import Data.ByteString.Lazy (fromStrict)
import Data.String qualified as HString
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Ledger.Crypto (Signature (Signature))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (LedgerBytes)
import Plutus.V2.Ledger.Tx (TxId (TxId), TxOutRef (TxOutRef))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import TrustlessSidechain.MerkleTree (MerkleProof (MerkleProof), RootHash (RootHash), Side (L, R), Up (Up), sibling, siblingSide)
import TrustlessSidechain.OffChain (showBuiltinBS)
import TrustlessSidechain.Types (
  BlockProducerRegistration (
    BlockProducerRegistration,
    bprEcdsaSecp256k1PubKey,
    bprInputUtxo,
    bprOwnPkh,
    bprSidechainSignature,
    bprSpoPubKey,
    bprSpoSignature
  ),
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmEcdsaSecp256k1PubKey,
    bprmInputUtxo,
    bprmSidechainParams
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
  EcdsaSecp256k1PubKey (EcdsaSecp256k1PubKey),
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

{- | Tests for all data types with @IsData@ implementation
 Some of the data types are only checked transitively (included by some other type)

 In order to regenerate golden tests files, simply delete the old ones in `./test/golden`
-}
tests :: TestTree
tests =
  testGroup
    "Golden tests of ToData encoded data"
    [ dataEncoderGoldenTest "SidechainParams" sampleSidechainParams
    , dataEncoderGoldenTest "CandidatePermissionMint" sampleCandidatePermissionMint
    , dataEncoderGoldenTest "BlockProducerRegistration" sampleBlockProducerRegistration
    , dataEncoderGoldenTest "BlockProducerRegistrationMsg" sampleBlockProducerRegistrationMsg
    , dataEncoderGoldenTest "MerkleTreeEntry" sampleMerkleTreeEntry
    , dataEncoderGoldenTest "MerkleRootInsertionMessage" sampleMerkleRootInsertionMessage
    , dataEncoderGoldenTest "SignedMerkleRoot" sampleSignedMerkleRoot
    , dataEncoderGoldenTest "SignedMerkleRootMint" sampleSignedMerkleRootMint
    , dataEncoderGoldenTest "CombinedMerkleProof" sampleCombinedMerkleProof
    , dataEncoderGoldenTest "FUELReedemer1" sampleFUELReedemer1
    , dataEncoderGoldenTest "FUELReedemer2" sampleFUELReedemer2
    , dataEncoderGoldenTest "FUELMint" sampleFUELMint
    , dataEncoderGoldenTest "UpdateCommitteeHashDatum" sampleUpdateCommitteeHashDatum
    , dataEncoderGoldenTest "UpdateCommitteeHashRedeemer" sampleUpdateCommitteeHashRedeemer
    , dataEncoderGoldenTest "UpdateCommitteeHash" sampleUpdateCommitteeHash
    , dataEncoderGoldenTest "UpdateCommitteeHashMessage" sampleUpdateCommitteeHashMessage
    , dataEncoderGoldenTest "CheckpointDatum" sampleCheckpointDatum
    , dataEncoderGoldenTest "CheckpointRedeemer" sampleCheckpointRedeemer
    , dataEncoderGoldenTest "CheckpointParameter" sampleCheckpointParameter
    , dataEncoderGoldenTest "CheckpointMessage" sampleCheckpointMessage
    ]

-- * Sample data - building blocks

sampleTxOutRef :: TxOutRef
sampleTxOutRef = TxOutRef (TxId "e41c9b57841e582c207bb68d5e9736fb48c7af5f1ec29ade00692fa5e0e47efa") 4

sampleCommitteePubKeys :: [EcdsaSecp256k1PubKey]
sampleCommitteePubKeys =
  [ EcdsaSecp256k1PubKey "02dbfc8b66c22f931a6647fd86db2fc073dd564b99837226a1bdfe7a99578854ec"
  , EcdsaSecp256k1PubKey "03e19ca8508c2bc8fc46872086bab3c0c91d65862525577034d25564c212d76ab3"
  , EcdsaSecp256k1PubKey "03cd0ea1b6652948b4a9c4551101981330feaea6a23e0698ad0b9d0adf05d4a260"
  , EcdsaSecp256k1PubKey "0240e20a9959ebf4fb9a59162ee324f95279c85e5277f98ef80dd3a77d4bc04b50"
  ]

sampleCommitteePubKeys' :: [EcdsaSecp256k1PubKey]
sampleCommitteePubKeys' =
  [ EcdsaSecp256k1PubKey "023a435665c40e4fb432790fb738be6c888f5d37e273a637a8a5b84ec285f52122"
  , EcdsaSecp256k1PubKey "02f45e4a11288fd16cb908c85410390d10129dae674dc800001c21a9fed2d59c2c"
  , EcdsaSecp256k1PubKey "0241ea46a78aef957c814f8aa5f64355ac8c3b59318d4eb3f2aacafcf724995513"
  , EcdsaSecp256k1PubKey "0253e0839b05b420879089621b60f4a9618e877a90f624a2d8c8e8afa17c8be624"
  ]

sampleCommitteeSignatures :: [LedgerBytes]
sampleCommitteeSignatures =
  [ "6fd0dd049dc90ebf5d52450e03bcd833ab53352f50bc15c7c2c1236b6aa78ff54fef9979d470bffb79ef949abc075bfb456fea4665f9b722d371f3301e05fd65"
  , "7026f80d62c4bdaa303bf94892fecb27a20a407209a9d321c3f34b82e73ab1fa3d12b627c8d44d9a2c1674e38e68d389e61a2a867f61074e64c9d7d37aaacd7e"
  , "44cf123d63075abf1cd1141b65a16f6f8e3f49c21f3c09661e0ed2a633f34f165fe982513ce82b51f4161c792877d0333b27cea6b413917f18738155988d18e3"
  ]

sampleMerkleProof :: MerkleProof
sampleMerkleProof =
  MerkleProof
    [ Up
        { siblingSide = L
        , sibling = RootHash "595a007f79ffff017f802effeb013f804935ff008054807f9a48e27f8c80004b"
        }
    , Up
        { siblingSide = R
        , sibling = RootHash "8073190a01517350690100944edbffffff01e54e130069ffeee4337f807fa0ff"
        }
    , Up
        { siblingSide = L
        , sibling = RootHash "00ffab800eff01ffc4ff8080ff77017b3d010100e60097010100ffd6ff3a0162"
        }
    , Up
        { siblingSide = L
        , sibling = RootHash "803d0ba3ff8080ff5cdf22dd00e38080807748fffd0078a59b80002964ff11c2"
        }
    , Up
        { siblingSide = R
        , sibling = RootHash "7b808fec00b2f580e101acb77f220180808035787380807f024d01d4b92ff301"
        }
    , Up
        { siblingSide = R
        , sibling = RootHash "a680e03c0001ea3e0016a9ac7f6c5be0017f66802b800180000001ff88e00079"
        }
    , Up
        { siblingSide = L
        , sibling = RootHash "a9920088807fa280997f26f1800180ff2f5ffe700032ff017f7f807280a0aa00"
        }
    ]

-- * Sample data - test subjects

sampleSidechainParams :: SidechainParams
sampleSidechainParams =
  SidechainParams
    { chainId = 11
    , genesisHash = GenesisHash "e8118a6a0f2ea8447b2418b0301fa53fa97f95a042fc92edbd7eda9f809d9040"
    , genesisUtxo = sampleTxOutRef
    , thresholdNumerator = 2
    , thresholdDenominator = 3
    }

sampleCandidatePermissionMint :: CandidatePermissionMint
sampleCandidatePermissionMint =
  CandidatePermissionMint
    { cpmSidechainParams = sampleSidechainParams
    , cpmUtxo = sampleTxOutRef
    }

sampleBlockProducerRegistration :: BlockProducerRegistration
sampleBlockProducerRegistration =
  BlockProducerRegistration
    { bprSpoPubKey = "e734ea6c2b6257de72355e472aa05a4c487e6b463c029ed306df2f01b5636b58"
    , bprEcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , bprSpoSignature = Signature "33a9681755ecdae6f572bcecaacb53d2fc6add491aa5dc65180195e73b87b8abcd0f0520ee808b31fe625631d5c86eda31b5dfe6bf6bb18f0391facd939f6d00"
    , bprSidechainSignature = Signature "b377dd97d20aaf784cf88dbbb1ffc0663311cb60451b5646c57192060143b9f6674f52aba3b7e09cc77eddafed0f64ca040dcdaa0c433ecb4b07a11b4b541000"
    , bprInputUtxo = sampleTxOutRef
    , bprOwnPkh = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    }

sampleBlockProducerRegistrationMsg :: BlockProducerRegistrationMsg
sampleBlockProducerRegistrationMsg =
  BlockProducerRegistrationMsg
    { bprmSidechainParams = sampleSidechainParams
    , bprmEcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey "02dbfc8b66c22f931a6647fd86db2fc073dd564b99837226a1bdfe7a99578854ec"
    , bprmInputUtxo = sampleTxOutRef
    }

sampleMerkleTreeEntry :: MerkleTreeEntry
sampleMerkleTreeEntry =
  MerkleTreeEntry
    { mteIndex = -8858258933817599851
    , mteAmount = 8887194232705394223
    , mteRecipient = "ecff7f9199faff168fb0015f01801b5e017f7fb2f3bdfc7fb58436d515000180"
    , mtePreviousMerkleRoot = Just "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
    }

sampleMerkleRootInsertionMessage :: MerkleRootInsertionMessage
sampleMerkleRootInsertionMessage =
  MerkleRootInsertionMessage
    { mrimSidechainParams = sampleSidechainParams
    , mrimMerkleRoot = "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
    , mrimPreviousMerkleRoot = Nothing
    }

sampleSignedMerkleRoot :: SignedMerkleRoot
sampleSignedMerkleRoot =
  SignedMerkleRoot
    { merkleRoot = "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
    , previousMerkleRoot = Nothing
    , signatures = sampleCommitteeSignatures
    , committeePubKeys = sampleCommitteePubKeys
    }

sampleSignedMerkleRootMint :: SignedMerkleRootMint
sampleSignedMerkleRootMint =
  SignedMerkleRootMint
    { smrmSidechainParams = sampleSidechainParams
    , smrmUpdateCommitteeHashCurrencySymbol = "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
    , smrmValidatorHash = "3689d804b3e43789fb0442314ba46aa1ccb9b3aa03fc5073ffa6486d"
    }

sampleCombinedMerkleProof :: CombinedMerkleProof
sampleCombinedMerkleProof =
  CombinedMerkleProof
    { cmpTransaction = sampleMerkleTreeEntry
    , cmpMerkleProof = sampleMerkleProof
    }

sampleFUELReedemer1 :: FUELRedeemer
sampleFUELReedemer1 =
  MainToSide "dbc05b1ecb4fdaef943819c0b04e9ef6df4babd6"

sampleFUELReedemer2 :: FUELRedeemer
sampleFUELReedemer2 =
  SideToMain sampleMerkleTreeEntry sampleMerkleProof

sampleFUELMint :: FUELMint
sampleFUELMint =
  FUELMint
    { fmMptRootTokenCurrencySymbol = "c446faf0e8117442c1ebbc9a3a5692e29ce1135df45c5d75eb63d672"
    , fmSidechainParams = sampleSidechainParams
    , fmDsKeyCurrencySymbol = "ba14173257eec781ca12722cd0b76274caa2a5300ca35e80a0a4f2d9"
    }

sampleUpdateCommitteeHashDatum :: UpdateCommitteeHashDatum
sampleUpdateCommitteeHashDatum =
  UpdateCommitteeHashDatum
    { committeeHash = "569f8aa770784cd10d0fe657fd389c76b20d3dafcd2fab43a14ebb2fd99e94e2"
    , sidechainEpoch = 12
    }

sampleUpdateCommitteeHashRedeemer :: UpdateCommitteeHashRedeemer
sampleUpdateCommitteeHashRedeemer =
  UpdateCommitteeHashRedeemer
    { committeeSignatures = sampleCommitteeSignatures
    , committeePubKeys = sampleCommitteePubKeys
    , newCommitteePubKeys = sampleCommitteePubKeys'
    , previousMerkleRoot = Just "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
    }

sampleUpdateCommitteeHash :: UpdateCommitteeHash
sampleUpdateCommitteeHash =
  UpdateCommitteeHash
    { cSidechainParams = sampleSidechainParams
    , cToken = Value.assetClass "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d" ""
    , cMptRootTokenCurrencySymbol = "c446faf0e8117442c1ebbc9a3a5692e29ce1135df45c5d75eb63d672"
    }

sampleUpdateCommitteeHashMessage :: UpdateCommitteeHashMessage
sampleUpdateCommitteeHashMessage =
  UpdateCommitteeHashMessage
    { uchmSidechainParams = sampleSidechainParams
    , uchmNewCommitteePubKeys = sampleCommitteePubKeys'
    , uchmPreviousMerkleRoot = Just "803399802c80ff3b7f82ff6f00d9887a51ff47ff7912ff15f10a84ff01ff7f01"
    , uchmSidechainEpoch = 12
    }

sampleCheckpointDatum :: CheckpointDatum
sampleCheckpointDatum =
  CheckpointDatum
    { checkpointBlockHash = "5560457708ed4dbfdd3be10a3fee66e22ffef3143e8a69fca64e06a4ac8b761e"
    , checkpointBlockNumber = 15791
    }

sampleCheckpointRedeemer :: CheckpointRedeemer
sampleCheckpointRedeemer =
  CheckpointRedeemer
    { checkpointCommitteeSignatures = sampleCommitteeSignatures
    , checkpointCommitteePubKeys = sampleCommitteePubKeys
    , newCheckpointBlockHash = "5560457708ed4dbfdd3be10a3fee66e22ffef3143e8a69fca64e06a4ac8b761e"
    , newCheckpointBlockNumber = 15791
    }

sampleCheckpointParameter :: CheckpointParameter
sampleCheckpointParameter =
  CheckpointParameter
    { checkpointSidechainParams = sampleSidechainParams
    , checkpointAssetClass = Value.assetClass "ba057436091a591a90329bd86e0e1617ac05cff039fb594b577a4084" ""
    , committeeHashAssetClass = Value.assetClass "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d" ""
    }

sampleCheckpointMessage :: CheckpointMessage
sampleCheckpointMessage =
  CheckpointMessage
    { checkpointMsgSidechainParams = sampleSidechainParams
    , checkpointMsgBlockHash = "5560457708ed4dbfdd3be10a3fee66e22ffef3143e8a69fca64e06a4ac8b761e"
    , checkpointMsgBlockNumber = 863548
    , checkpointMsgSidechainEpoch = 15791
    }

{- | Creating a test group with two golden tests:
 - encoding data using `toBuiltinData`
 - serialising BuiltinData to CBOR

 Results of the tests are compared to the files under ./test/golden/*.golden
 If no file exists for the given data type, a new one will be created automatically
-}
dataEncoderGoldenTest :: ToData a => HString.String -> a -> TestTree
dataEncoderGoldenTest name sampleData =
  let builtinData = toBuiltinData sampleData
      plutusDataBS = fromStrict $ encodeUtf8 $ Text.pack $ show builtinData
      cborBS = fromStrict $ encodeUtf8 $ Text.pack $ showBuiltinBS $ Builtins.serialiseData builtinData
   in testGroup
        ("Serialising " <> name)
        [ goldenVsString "IsData encoding" ("./test/golden/" <> name <> "-isdata.golden") $ pure plutusDataBS
        , goldenVsString "CBOR encoding" ("./test/golden/" <> name <> "-cbor.golden") $ pure cborBS
        ]
