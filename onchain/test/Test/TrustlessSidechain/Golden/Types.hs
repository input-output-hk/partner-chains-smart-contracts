{-# LANGUAGE DuplicateRecordFields #-}

module Test.TrustlessSidechain.Golden.Types (tests) where

import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (POSIXTime (..), toBuiltinData)
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.Governance.MultiSig (
  MultiSigGovParams (..),
 )
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.Types (
  IlliquidCirculationSupplyRedeemer (
    DepositMoreToSupply,
    WithdrawFromSupply
  ),
  ImmutableReserveSettings (
    ImmutableReserveSettings
  ),
  MutableReserveSettings (MutableReserveSettings),
  PermissionedCandidatesPolicyRedeemer (
    PermissionedCandidatesBurn,
    PermissionedCandidatesMint
  ),
  PermissionedCandidatesValidatorRedeemer (
    RemovePermissionedCandidates,
    UpdatePermissionedCandidates
  ),
  ReserveDatum (ReserveDatum, immutableSettings, mutableSettings, stats),
  ReserveRedeemer (
    DepositToReserve,
    Handover,
    TransferToIlliquidCirculationSupply,
    UpdateReserve
  ),
  ReserveStats (ReserveStats),
  VersionedGenericDatum (..),
 )
import Prelude (String)

-- | Tests for all data types with @IsData@ implementation
-- Some of the data types are only checked transitively (included by some other type)
--
-- In order to regenerate golden tests files, simply delete the old ones in `./test/golden`
tests :: TestTree
tests =
  testGroup
    "Golden tests for Types module"
    [ dataEncoderGoldenTest "PermissionedCandidatesPolicyRedeemer1" samplePermissionedCandidatesPolicyRedeemer1
    , dataEncoderGoldenTest "PermissionedCandidatesPolicyRedeemer2" samplePermissionedCandidatesPolicyRedeemer2
    , dataEncoderGoldenTest "PermissionedCandidatesValidatorRedeemer1" samplePermissionedCandidatesValidatorRedeemer1
    , dataEncoderGoldenTest "PermissionedCandidatesValidatorRedeemer2" samplePermissionedCandidatesValidatorRedeemer2
    , dataEncoderGoldenTest "ReserveDatum" sampleReserveDatum
    , dataEncoderGoldenTest "ReserveRedeemer1" sampleReserveRedeemer1
    , dataEncoderGoldenTest "ReserveRedeemer2" sampleReserveRedeemer2
    , dataEncoderGoldenTest "ReserveRedeemer3" sampleReserveRedeemer3
    , dataEncoderGoldenTest "ReserveRedeemer4" sampleReserveRedeemer4
    , dataEncoderGoldenTest "IlliquidCirculationSupplyRedeemer1" sampleIlliquidCirculationSupplyRedeemer1
    , dataEncoderGoldenTest "IlliquidCirculationSupplyRedeemer2" sampleIlliquidCirculationSupplyRedeemer2
    , dataEncoderGoldenTest "MultiSigGovParams" sampleMultiSigGovParams
    ]

-- * Sample data - building blocks

samplePermissionedCandidatesPolicyRedeemer1 :: PermissionedCandidatesPolicyRedeemer
samplePermissionedCandidatesPolicyRedeemer1 = PermissionedCandidatesMint

samplePermissionedCandidatesPolicyRedeemer2 :: PermissionedCandidatesPolicyRedeemer
samplePermissionedCandidatesPolicyRedeemer2 = PermissionedCandidatesBurn

samplePermissionedCandidatesValidatorRedeemer1 :: PermissionedCandidatesValidatorRedeemer
samplePermissionedCandidatesValidatorRedeemer1 = UpdatePermissionedCandidates

samplePermissionedCandidatesValidatorRedeemer2 :: PermissionedCandidatesValidatorRedeemer
samplePermissionedCandidatesValidatorRedeemer2 = RemovePermissionedCandidates

hexToBSUnsafe :: String -> ByteString
hexToBSUnsafe str =
  let bs = BC.pack str
   in case B16.decode bs of
        Right raw -> raw
        Left e -> error e

sampleReserveDatum :: VersionedGenericDatum ReserveDatum
sampleReserveDatum =
  VersionedGenericDatum
    { datum =
        ReserveDatum
          { immutableSettings =
              ImmutableReserveSettings
                (POSIXTime 1234513245)
                (Value.AssetClass (Value.currencySymbol (hexToBSUnsafe "0281158622b7d2eb738b885e1cca50218fb36ab4dc39014b83286b8ed95c78789d"), Value.tokenName "asdf"))
          , mutableSettings =
              MutableReserveSettings (Value.currencySymbol (hexToBSUnsafe "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d")) 0
          , stats = ReserveStats 15
          }
    , genericData = toBuiltinData ()
    , version = 0
    }

sampleReserveRedeemer1 :: ReserveRedeemer
sampleReserveRedeemer1 = DepositToReserve

sampleReserveRedeemer2 :: ReserveRedeemer
sampleReserveRedeemer2 = TransferToIlliquidCirculationSupply

sampleReserveRedeemer3 :: ReserveRedeemer
sampleReserveRedeemer3 = UpdateReserve

sampleReserveRedeemer4 :: ReserveRedeemer
sampleReserveRedeemer4 = Handover

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
