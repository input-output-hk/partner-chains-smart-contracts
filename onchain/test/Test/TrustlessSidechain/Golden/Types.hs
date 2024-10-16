{-# LANGUAGE DuplicateRecordFields #-}

module Test.TrustlessSidechain.Golden.Types (tests) where

import TrustlessSidechain.HaskellPrelude

import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (POSIXTime (..), TxOutRef (TxOutRef))
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.Governance.Admin (mkGovernanceAuthority)
import TrustlessSidechain.Governance.MultiSig (
  MultiSigGovParams (..),
  MultiSigGovRedeemer (
    MultiSigTokenGC,
    MultiSignatureCheck
  ),
 )
import TrustlessSidechain.Types (
  BlockProducerRegistration (
    BlockProducerRegistration,
    auraKey,
    grandpaKey,
    inputUtxo,
    ownPkh,
    sidechainPubKey,
    sidechainSignature,
    stakeOwnership
  ),
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    inputUtxo,
    sidechainParams,
    sidechainPubKey
  ),
  DParameterValidatorDatum (
    DParameterValidatorDatum,
    permissionedCandidatesCount,
    registeredCandidatesCount
  ),
  IlliquidCirculationSupplyRedeemer (
    DepositMoreToSupply,
    WithdrawFromSupply
  ),
  ImmutableReserveSettings (
    ImmutableReserveSettings
  ),
  InitTokenAssetClass (
    InitTokenAssetClass,
    initTokenCurrencySymbol,
    initTokenName
  ),
  InitTokenRedeemer (
    BurnInitToken,
    MintInitToken
  ),
  MutableReserveSettings (MutableReserveSettings),
  PermissionedCandidateKeys (
    PermissionedCandidateKeys,
    auraKey,
    grandpaKey,
    sidechainKey
  ),
  PermissionedCandidatesPolicyRedeemer (
    PermissionedCandidatesBurn,
    PermissionedCandidatesMint
  ),
  PermissionedCandidatesValidatorDatum (
    PermissionedCandidatesValidatorDatum,
    candidates
  ),
  PermissionedCandidatesValidatorRedeemer (
    RemovePermissionedCandidates,
    UpdatePermissionedCandidates
  ),
  ReserveAuthPolicyRedeemer (ReserveAuthPolicyRedeemer),
  ReserveDatum (ReserveDatum, immutableSettings, mutableSettings, stats),
  ReserveRedeemer (
    DepositToReserve,
    Handover,
    TransferToIlliquidCirculationSupply,
    UpdateReserve
  ),
  ReserveStats (ReserveStats),
  SidechainParams (
    SidechainParams,
    chainId,
    genesisUtxo,
    governanceAuthority,
    thresholdDenominator,
    thresholdNumerator
  ),
  StakeOwnership (AdaBasedStaking, TokenBasedStaking),
 )

-- | Tests for all data types with @IsData@ implementation
-- Some of the data types are only checked transitively (included by some other type)
--
-- In order to regenerate golden tests files, simply delete the old ones in `./test/golden`
tests :: TestTree
tests =
  testGroup
    "Golden tests for Types module"
    [ dataEncoderGoldenTest "SidechainParams" sampleSidechainParams
    , dataEncoderGoldenTest "BlockProducerRegistration1" sampleBlockProducerRegistration1
    , dataEncoderGoldenTest "BlockProducerRegistration2" sampleBlockProducerRegistration2
    , dataEncoderGoldenTest "BlockProducerRegistrationMsg" sampleBlockProducerRegistrationMsg
    , dataEncoderGoldenTest "DParameterValidatorDatum" sampleDParameterValidatorDatum
    , dataEncoderGoldenTest "PermissionedCandidatesPolicyRedeemer1" samplePermissionedCandidatesPolicyRedeemer1
    , dataEncoderGoldenTest "PermissionedCandidatesPolicyRedeemer2" samplePermissionedCandidatesPolicyRedeemer2
    , dataEncoderGoldenTest "PermissionedCandidateKeys" samplePermissionedCandidateKeys
    , dataEncoderGoldenTest "PermissionedCandidatesValidatorDatum" samplePermissionedCandidatesValidatorDatum
    , dataEncoderGoldenTest "PermissionedCandidatesValidatorRedeemer1" samplePermissionedCandidatesValidatorRedeemer1
    , dataEncoderGoldenTest "PermissionedCandidatesValidatorRedeemer2" samplePermissionedCandidatesValidatorRedeemer2
    , dataEncoderGoldenTest "InitTokenRedeemer1" sampleInitTokenRedeemer1
    , dataEncoderGoldenTest "InitTokenRedeemer2" sampleInitTokenRedeemer2
    , dataEncoderGoldenTest "InitTokenAssetClass" sampleInitTokenAssetClass
    , dataEncoderGoldenTest "ReserveDatum" sampleReserveDatum
    , dataEncoderGoldenTest "ReserveRedeemer1" sampleReserveRedeemer1
    , dataEncoderGoldenTest "ReserveRedeemer2" sampleReserveRedeemer2
    , dataEncoderGoldenTest "ReserveRedeemer3" sampleReserveRedeemer3
    , dataEncoderGoldenTest "ReserveRedeemer4" sampleReserveRedeemer4
    , dataEncoderGoldenTest "ReserveAuthPolicyRedeemer" sampleReserveAuthPolicyRedeemer
    , dataEncoderGoldenTest "IlliquidCirculationSupplyRedeemer1" sampleIlliquidCirculationSupplyRedeemer1
    , dataEncoderGoldenTest "IlliquidCirculationSupplyRedeemer2" sampleIlliquidCirculationSupplyRedeemer2
    , dataEncoderGoldenTest "MultiSigGovParams" sampleMultiSigGovParams
    , dataEncoderGoldenTest "MultiSigGovRedeemer1" sampleMultiSigGovRedeemer1
    , dataEncoderGoldenTest "MultiSigGovRedeemer2" sampleMultiSigGovRedeemer2
    ]

-- * Sample data - building blocks

sampleTxOutRef :: TxOutRef
sampleTxOutRef = TxOutRef "e41c9b57841e582c207bb68d5e9736fb48c7af5f1ec29ade00692fa5e0e47efa" 4

-- * Sample data - test subjects

sampleSidechainParams :: SidechainParams
sampleSidechainParams =
  SidechainParams
    { chainId = 11
    , genesisUtxo = sampleTxOutRef
    , governanceAuthority = mkGovernanceAuthority "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
    , thresholdNumerator = 2
    , thresholdDenominator = 3
    }

sampleBlockProducerRegistration1 :: BlockProducerRegistration
sampleBlockProducerRegistration1 =
  BlockProducerRegistration
    { stakeOwnership = AdaBasedStaking "e734ea6c2b6257de72355e472aa05a4c487e6b463c029ed306df2f01b5636b58" "33a9681755ecdae6f572bcecaacb53d2fc6add491aa5dc65180195e73b87b8abcd0f0520ee808b31fe625631d5c86eda31b5dfe6bf6bb18f0391facd939f6d00"
    , sidechainPubKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , sidechainSignature = "b377dd97d20aaf784cf88dbbb1ffc0663311cb60451b5646c57192060143b9f6674f52aba3b7e09cc77eddafed0f64ca040dcdaa0c433ecb4b07a11b4b541000"
    , inputUtxo = sampleTxOutRef
    , ownPkh = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    , auraKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , grandpaKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    }

sampleBlockProducerRegistration2 :: BlockProducerRegistration
sampleBlockProducerRegistration2 =
  BlockProducerRegistration
    { stakeOwnership = TokenBasedStaking
    , sidechainPubKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , sidechainSignature = "b377dd97d20aaf784cf88dbbb1ffc0663311cb60451b5646c57192060143b9f6674f52aba3b7e09cc77eddafed0f64ca040dcdaa0c433ecb4b07a11b4b541000"
    , inputUtxo = sampleTxOutRef
    , ownPkh = "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
    , auraKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , grandpaKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    }

sampleBlockProducerRegistrationMsg :: BlockProducerRegistrationMsg
sampleBlockProducerRegistrationMsg =
  BlockProducerRegistrationMsg
    { sidechainParams = sampleSidechainParams
    , sidechainPubKey = "02dbfc8b66c22f931a6647fd86db2fc073dd564b99837226a1bdfe7a99578854ec"
    , inputUtxo = sampleTxOutRef
    }

sampleDParameterValidatorDatum :: DParameterValidatorDatum
sampleDParameterValidatorDatum =
  DParameterValidatorDatum
    { permissionedCandidatesCount = 17
    , registeredCandidatesCount = 42
    }

samplePermissionedCandidatesPolicyRedeemer1 :: PermissionedCandidatesPolicyRedeemer
samplePermissionedCandidatesPolicyRedeemer1 = PermissionedCandidatesMint

samplePermissionedCandidatesPolicyRedeemer2 :: PermissionedCandidatesPolicyRedeemer
samplePermissionedCandidatesPolicyRedeemer2 = PermissionedCandidatesBurn

samplePermissionedCandidateKeys :: PermissionedCandidateKeys
samplePermissionedCandidateKeys =
  PermissionedCandidateKeys
    { sidechainKey = "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"
    , auraKey = "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
    , grandpaKey = "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
    }

samplePermissionedCandidatesValidatorDatum :: PermissionedCandidatesValidatorDatum
samplePermissionedCandidatesValidatorDatum =
  PermissionedCandidatesValidatorDatum
    { candidates =
        [ samplePermissionedCandidateKeys
        , samplePermissionedCandidateKeys
        ]
    }

samplePermissionedCandidatesValidatorRedeemer1 :: PermissionedCandidatesValidatorRedeemer
samplePermissionedCandidatesValidatorRedeemer1 = UpdatePermissionedCandidates

samplePermissionedCandidatesValidatorRedeemer2 :: PermissionedCandidatesValidatorRedeemer
samplePermissionedCandidatesValidatorRedeemer2 = RemovePermissionedCandidates

sampleInitTokenRedeemer1 :: InitTokenRedeemer
sampleInitTokenRedeemer1 = MintInitToken

sampleInitTokenRedeemer2 :: InitTokenRedeemer
sampleInitTokenRedeemer2 = BurnInitToken

sampleInitTokenAssetClass :: InitTokenAssetClass
sampleInitTokenAssetClass =
  InitTokenAssetClass
    { initTokenCurrencySymbol = "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
    , initTokenName = "foo bar"
    }

sampleReserveDatum :: ReserveDatum
sampleReserveDatum =
  ReserveDatum
    { immutableSettings =
        ImmutableReserveSettings
          (POSIXTime 1234513245)
          (Value.AssetClass ("0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d", "asdf"))
    , mutableSettings =
        MutableReserveSettings "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d" 0
    , stats = ReserveStats 15
    }

sampleReserveRedeemer1 :: ReserveRedeemer
sampleReserveRedeemer1 = DepositToReserve 1

sampleReserveRedeemer2 :: ReserveRedeemer
sampleReserveRedeemer2 = TransferToIlliquidCirculationSupply

sampleReserveRedeemer3 :: ReserveRedeemer
sampleReserveRedeemer3 = UpdateReserve 1

sampleReserveRedeemer4 :: ReserveRedeemer
sampleReserveRedeemer4 = Handover 1

sampleReserveAuthPolicyRedeemer :: ReserveAuthPolicyRedeemer
sampleReserveAuthPolicyRedeemer = ReserveAuthPolicyRedeemer 1

sampleIlliquidCirculationSupplyRedeemer1 :: IlliquidCirculationSupplyRedeemer
sampleIlliquidCirculationSupplyRedeemer1 = DepositMoreToSupply

sampleIlliquidCirculationSupplyRedeemer2 :: IlliquidCirculationSupplyRedeemer
sampleIlliquidCirculationSupplyRedeemer2 = WithdrawFromSupply

sampleMultiSigGovParams :: MultiSigGovParams
sampleMultiSigGovParams =
  MultiSigGovParams
    { governanceMembers =
        [ "0f45aaf1b2959db6e5ff94dbb1f823bf257680c3c723ac2d49f97546"
        , "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
        , "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
        ]
    , requiredSignatures = 2
    }

sampleMultiSigGovRedeemer1 :: MultiSigGovRedeemer
sampleMultiSigGovRedeemer1 = MultiSignatureCheck

sampleMultiSigGovRedeemer2 :: MultiSigGovRedeemer
sampleMultiSigGovRedeemer2 = MultiSigTokenGC
