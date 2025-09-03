{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.ByteString (ByteString)
import GHC.Exts (fromList)
import Laws (toDataSafeLaws', toDataUnsafeLaws')
import PlutusLedgerApi.Data.V2 (
  CurrencySymbol (CurrencySymbol),
  PubKeyHash (PubKeyHash),
 )
import PlutusLedgerApi.V1.Data.Value (AssetClass (AssetClass), adaToken)
import PlutusTx.Prelude qualified as PTPrelude
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  arbitrary,
  oneof,
  shrink,
  vectorOf,
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)
import TrustlessSidechain.Types (
  IlliquidCirculationSupplyRedeemer (DepositMoreToSupply, WithdrawFromSupply),
  ImmutableReserveSettings (ImmutableReserveSettings),
  MutableReserveSettings (MutableReserveSettings),
  PermissionedCandidatesPolicyRedeemer (PermissionedCandidatesBurn, PermissionedCandidatesMint),
  PermissionedCandidatesValidatorRedeemer (RemovePermissionedCandidates, UpdatePermissionedCandidates),
  ReserveDatum (ReserveDatum),
  ReserveRedeemer (DepositToReserve, Handover, TransferToIlliquidCirculationSupply, UpdateReserve),
  ReserveStats (ReserveStats),
  VersionOracle (VersionOracle),
  VersionOracleConfig (VersionOracleConfig),
 )

main :: IO ()
main =
  defaultMain
    . adjustOption go
    . testGroup "Roundtrip"
    $ [ testProperty "VersionOracle (safe)" . toDataSafeLaws' genVO shrinkVO $ show
      , testProperty "VersionOracle (unsafe)" . toDataUnsafeLaws' genVO shrinkVO $ show
      , testProperty "VersionOracleConfig (safe)" . toDataSafeLaws' genVOC shrinkVOC $ show
      , testProperty "VersionOracleConfig (unsafe)" . toDataUnsafeLaws' genVOC shrinkVOC $ show
      , testProperty "PermissionedCandidatesPolicyRedeemer (safe)" . toDataSafeLaws' genPCPR shrinkPCPR $ show
      , testProperty "PermissionedCandidatesPolicyRedeemer (unsafe)" . toDataUnsafeLaws' genPCPR shrinkPCPR $ show
      , testProperty "PermissionedCandidatesValidatorRedeemer (safe)" . toDataSafeLaws' genPCVR shrinkPCVR $ show
      , testProperty "PermissionedCandidatesValidatorRedeemer (unsafe)" . toDataUnsafeLaws' genPCVR shrinkPCVR $ show
      , testProperty "ReserveDatum (safe)" . toDataSafeLaws' genRD shrinkRD $ show
      , testProperty "ReserveDatum (unsafe)" . toDataUnsafeLaws' genRD shrinkRD $ show
      , testProperty "ReserveRedeemer (safe)" . toDataSafeLaws' genRR shrinkRR $ show
      , testProperty "ReserveRedeemer (unsafe)" . toDataUnsafeLaws' genRR shrinkRR $ show
      , testProperty "IlliquidCirculationSupplyRedeemer (safe)" . toDataSafeLaws' genICSR shrinkICSR $ show
      , testProperty "IlliquidCirculationSupplyRedeemer (unsafe)" . toDataUnsafeLaws' genICSR shrinkICSR $ show
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max (QuickCheckTests 10_000)

-- Helpers

-- Generators

genPCPR :: Gen PermissionedCandidatesPolicyRedeemer
genPCPR = oneof [pure PermissionedCandidatesMint, pure PermissionedCandidatesBurn]

genPCVR :: Gen PermissionedCandidatesValidatorRedeemer
genPCVR = oneof [pure UpdatePermissionedCandidates, pure RemovePermissionedCandidates]

genRD :: Gen ReserveDatum
genRD = do
  ArbitraryCurrencySymbol cs1 <- arbitrary
  ArbitraryCurrencySymbol cs2 <- arbitrary
  i <- arbitrary
  c <- arbitrary

  pure $
    ReserveDatum
      (ImmutableReserveSettings (AssetClass (cs1, adaToken)))
      (MutableReserveSettings cs2 i)
      (ReserveStats c)

genRR :: Gen ReserveRedeemer
genRR = do
  oneof
    [ pure DepositToReserve
    , pure TransferToIlliquidCirculationSupply
    , pure UpdateReserve
    , pure Handover
    ]

genICSR :: Gen IlliquidCirculationSupplyRedeemer
genICSR =
  oneof
    [ pure DepositMoreToSupply
    , pure WithdrawFromSupply
    ]

-- Generates arbitrary bytes
genVO :: Gen VersionOracle
genVO = VersionOracle <$> arbitrary

genVOC :: Gen VersionOracleConfig
genVOC =
  VersionOracleConfig <$> do
    ArbitraryCurrencySymbol sym <- arbitrary
    pure sym

-- Shrinkers

shrinkPCPR :: PermissionedCandidatesPolicyRedeemer -> [PermissionedCandidatesPolicyRedeemer]
shrinkPCPR = const []

shrinkPCVR :: PermissionedCandidatesValidatorRedeemer -> [PermissionedCandidatesValidatorRedeemer]
shrinkPCVR = const []

shrinkRD :: ReserveDatum -> [ReserveDatum]
shrinkRD = const []

shrinkRR :: ReserveRedeemer -> [ReserveRedeemer]
shrinkRR = const []

shrinkICSR :: IlliquidCirculationSupplyRedeemer -> [IlliquidCirculationSupplyRedeemer]
shrinkICSR = const []

shrinkVO :: VersionOracle -> [VersionOracle]
shrinkVO (VersionOracle scriptID) = do
  sid <- shrink scriptID
  pure $ VersionOracle sid

shrinkVOC :: VersionOracleConfig -> [VersionOracleConfig]
shrinkVOC (VersionOracleConfig versionOracleCurrencySymbol) = do
  ArbitraryCurrencySymbol sym <- shrink (ArbitraryCurrencySymbol versionOracleCurrencySymbol)
  pure $ VersionOracleConfig sym

-- | Wrapper for 'PubKeyHash' to provide QuickCheck instances.
newtype ArbitraryPubKeyHash = ArbitraryPubKeyHash PubKeyHash
  deriving
    ( Eq
    , Ord
    , PTPrelude.Eq
    , PTPrelude.Ord
    )
    via PubKeyHash
  deriving stock
    ( Show
    )

-- | Does not shrink, as it doesn't make much sense to.
instance Arbitrary ArbitraryPubKeyHash where
  arbitrary =
    ArbitraryPubKeyHash
      . PubKeyHash
      . PTPrelude.toBuiltin
      . fromList @ByteString
      <$> vectorOf 28 arbitrary

-- | Wrapper for 'CurrencySymbol' to provide QuickCheck instances.
newtype ArbitraryCurrencySymbol = ArbitraryCurrencySymbol CurrencySymbol
  deriving
    ( Eq
    , Ord
    , PTPrelude.Eq
    , PTPrelude.Ord
    )
    via CurrencySymbol
  deriving stock
    ( Show
    )

-- | This does /not/ generate the ADA symbol. Does not shrink (it wouldn't make
-- much sense to).
instance Arbitrary ArbitraryCurrencySymbol where
  arbitrary =
    ArbitraryCurrencySymbol
      . CurrencySymbol
      . PTPrelude.toBuiltin
      . fromList @ByteString
      <$> vectorOf 28 arbitrary
