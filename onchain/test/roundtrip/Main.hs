{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Crypto.Secp256k1 qualified as SECP
import GHC.Exts (fromList)
import Laws (toDataSafeLaws', toDataUnsafeLaws')
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
import PlutusLedgerApi.V2 (
  LedgerBytes (LedgerBytes),
  POSIXTime (POSIXTime),
 )
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  arbitrary,
  chooseInteger,
  liftArbitrary,
  liftShrink,
  oneof,
  shrink,
  vectorOf,
 )
import Test.QuickCheck.Extra (
  ArbitraryBytes (ArbitraryBytes),
  ArbitraryCurrencySymbol (ArbitraryCurrencySymbol),
  ArbitraryPubKeyHash (ArbitraryPubKeyHash),
  ArbitraryTxOutRef (ArbitraryTxOutRef),
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)
import TrustlessSidechain.Governance.Admin (GovernanceAuthority (GovernanceAuthority))
import TrustlessSidechain.Governance.MultiSig (
  MultiSigGovParams (MultiSigGovParams),
 )
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.PlutusPrelude qualified as PTPrelude
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
  DParameterValidatorDatum (DParameterValidatorDatum),
  EcdsaSecp256k1PubKey (EcdsaSecp256k1PubKey),
  IlliquidCirculationSupplyRedeemer (DepositMoreToSupply, WithdrawFromSupply),
  ImmutableReserveSettings (ImmutableReserveSettings),
  MutableReserveSettings (MutableReserveSettings),
  PermissionedCandidateKeys (PermissionedCandidateKeys),
  PermissionedCandidatesPolicyRedeemer (PermissionedCandidatesBurn, PermissionedCandidatesMint),
  PermissionedCandidatesValidatorDatum (PermissionedCandidatesValidatorDatum),
  PermissionedCandidatesValidatorRedeemer (RemovePermissionedCandidates, UpdatePermissionedCandidates),
  PubKey (PubKey),
  ReserveDatum (ReserveDatum),
  ReserveRedeemer (DepositToReserve, Handover, TransferToIlliquidCirculationSupply, UpdateReserve),
  ReserveStats (ReserveStats),
  SidechainParams (
    SidechainParams,
    genesisUtxo,
    governanceAuthority
  ),
  Signature (Signature),
  StakeOwnership (AdaBasedStaking, TokenBasedStaking),
 )
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle),
  VersionOracleConfig (VersionOracleConfig),
 )

main :: IO ()
main =
  defaultMain
    . adjustOption go
    . testGroup "Roundtrip"
    $ [ testProperty "SidechainParams (safe)" . toDataSafeLaws' genSP shrinkSP $ show
      , testProperty "SidechainParams (unsafe)" . toDataUnsafeLaws' genSP shrinkSP $ show
      , testProperty "EcdsaSecp256k1PubKey" . toDataSafeLaws' genPK shrinkPK $ show
      , testProperty "EcdsaSecp256k1PubKey" . toDataUnsafeLaws' genPK shrinkPK $ show
      , testProperty "BlockProducerRegistration (safe)" . toDataSafeLaws' genBPR shrinkBPR $ show
      , testProperty "BlockProducerRegistration (unsafe)" . toDataUnsafeLaws' genBPR shrinkBPR $ show
      , testProperty "BlockProducerRegistrationMsg (safe)" . toDataSafeLaws' genBPRM shrinkBPRM $ show
      , testProperty "BlockProducerRegistrationMsg (unsafe)" . toDataUnsafeLaws' genBPRM shrinkBPRM $ show
      , testProperty "VersionOracle (safe)" . toDataSafeLaws' genVO shrinkVO $ show
      , testProperty "VersionOracle (unsafe)" . toDataUnsafeLaws' genVO shrinkVO $ show
      , testProperty "VersionOracleConfig (safe)" . toDataSafeLaws' genVOC shrinkVOC $ show
      , testProperty "VersionOracleConfig (unsafe)" . toDataUnsafeLaws' genVOC shrinkVOC $ show
      , testProperty "DParameterValidatorDatum (safe)" . toDataSafeLaws' genDPVD shrinkDPVD $ show
      , testProperty "DParameterValidatorDatum (unsafe)" . toDataUnsafeLaws' genDPVD shrinkDPVD $ show
      , testProperty "PermissionedCandidateKeys (safe)" . toDataSafeLaws' genPCK shrinkPCK $ show
      , testProperty "PermissionedCandidateKeys (unsafe)" . toDataUnsafeLaws' genPCK shrinkPCK $ show
      , testProperty "PermissionedCandidatesPolicyRedeemer (safe)" . toDataSafeLaws' genPCPR shrinkPCPR $ show
      , testProperty "PermissionedCandidatesPolicyRedeemer (unsafe)" . toDataUnsafeLaws' genPCPR shrinkPCPR $ show
      , testProperty "PermissionedCandidatesValidatorDatum (safe)" . toDataSafeLaws' genPCVD shrinkPCVD $ show
      , testProperty "PermissionedCandidatesValidatorDatum (unsafe)" . toDataUnsafeLaws' genPCVD shrinkPCVD $ show
      , testProperty "PermissionedCandidatesValidatorRedeemer (safe)" . toDataSafeLaws' genPCVR shrinkPCVR $ show
      , testProperty "PermissionedCandidatesValidatorRedeemer (unsafe)" . toDataUnsafeLaws' genPCVR shrinkPCVR $ show
      , testProperty "ReserveDatum (safe)" . toDataSafeLaws' genRD shrinkRD $ show
      , testProperty "ReserveDatum (unsafe)" . toDataUnsafeLaws' genRD shrinkRD $ show
      , testProperty "ReserveRedeemer (safe)" . toDataSafeLaws' genRR shrinkRR $ show
      , testProperty "ReserveRedeemer (unsafe)" . toDataUnsafeLaws' genRR shrinkRR $ show
      , testProperty "IlliquidCirculationSupplyRedeemer (safe)" . toDataSafeLaws' genICSR shrinkICSR $ show
      , testProperty "IlliquidCirculationSupplyRedeemer (unsafe)" . toDataUnsafeLaws' genICSR shrinkICSR $ show
      , testProperty "MultiSigGovParams (safe)" . toDataSafeLaws' genMSGP shrinkMSGP $ show
      , testProperty "MultiSigGovParams (unsafe)" . toDataUnsafeLaws' genMSGP shrinkMSGP $ show
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max (QuickCheckTests 10_000)

-- Helpers

-- Generators

genDPVD :: Gen DParameterValidatorDatum
genDPVD = DParameterValidatorDatum <$> arbitrary <*> arbitrary

genPCK :: Gen PermissionedCandidateKeys
genPCK = do
  ArbitraryBytes sk <- arbitrary
  ArbitraryBytes ak <- arbitrary
  ArbitraryBytes gk <- arbitrary
  pure $ PermissionedCandidateKeys sk ak gk

genPCPR :: Gen PermissionedCandidatesPolicyRedeemer
genPCPR = oneof [pure PermissionedCandidatesMint, pure PermissionedCandidatesBurn]

genPCVD :: Gen PermissionedCandidatesValidatorDatum
genPCVD = PermissionedCandidatesValidatorDatum <$> liftArbitrary genPCK

genPCVR :: Gen PermissionedCandidatesValidatorRedeemer
genPCVR = oneof [pure UpdatePermissionedCandidates, pure RemovePermissionedCandidates]

genRD :: Gen ReserveDatum
genRD = do
  pt <- arbitrary
  ArbitraryCurrencySymbol cs1 <- arbitrary
  ArbitraryCurrencySymbol cs2 <- arbitrary
  i <- arbitrary
  c <- arbitrary

  pure
    $ ReserveDatum
      (ImmutableReserveSettings (POSIXTime pt) (AssetClass (cs1, "")))
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

genMSGP :: Gen MultiSigGovParams
genMSGP = do
  pkhs <- liftArbitrary $ do
    ArbitraryPubKeyHash pkh <- arbitrary
    pure pkh
  a <- chooseInteger (1, fromIntegral $ length pkhs)
  pure $ MultiSigGovParams pkhs a

-- Generates arbitrary bytes
genVO :: Gen VersionOracle
genVO = VersionOracle <$> arbitrary

genVOC :: Gen VersionOracleConfig
genVOC =
  VersionOracleConfig <$> do
    ArbitraryCurrencySymbol sym <- arbitrary
    pure sym

genBPRM :: Gen BlockProducerRegistrationMsg
genBPRM = do
  sp <- genSP
  spk <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  ArbitraryTxOutRef tout <- arbitrary
  pure . BlockProducerRegistrationMsg sp spk $ tout

genSO :: Gen StakeOwnership
genSO = oneof [arbitraryAdaBasedCreds, pure TokenBasedStaking]
  where
    arbitraryAdaBasedCreds = do
      ArbitraryPubKey pk <- arbitrary
      ArbitrarySignature sig <- arbitrary
      pure $ AdaBasedStaking pk sig

genBPR :: Gen BlockProducerRegistration
genBPR = do
  so <- genSO
  sidePk <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  auraKey <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  grandpaKey <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  ArbitrarySignature sideSig <- arbitrary
  ArbitraryTxOutRef iu <- arbitrary
  ArbitraryPubKeyHash pkh <- arbitrary
  pure
    $ BlockProducerRegistration
      so
      sidePk
      sideSig
      iu
      pkh
      auraKey
      grandpaKey

-- | A local context.
{-# NOINLINE ctx #-}
ctx :: SECP.Ctx
ctx = unsafePerformIO SECP.createContext

genPK :: Gen EcdsaSecp256k1PubKey
genPK = do
  seed <- fromList @ByteString <$> vectorOf 32 arbitrary
  case SECP.secKey seed of
    Just privKey ->
      pure
        . EcdsaSecp256k1PubKey
        . LedgerBytes
        . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
        . SECP.exportPubKey ctx True
        . SECP.derivePubKey ctx
        $ privKey
    Nothing -> genPK -- we assume this isn't gonna happen too often

genGA :: Gen GovernanceAuthority
genGA = do
  ArbitraryPubKeyHash pkh <- arbitrary
  pure $ GovernanceAuthority pkh

genSP :: Gen SidechainParams
genSP = do
  ArbitraryTxOutRef gu <- arbitrary
  ga <- genGA
  pure . SidechainParams gu $ ga

-- Shrinkers

shrinkDPVD :: DParameterValidatorDatum -> [DParameterValidatorDatum]
shrinkDPVD (DParameterValidatorDatum pcc rcc) = DParameterValidatorDatum <$> shrink pcc <*> shrink rcc

shrinkPCK :: PermissionedCandidateKeys -> [PermissionedCandidateKeys]
shrinkPCK (PermissionedCandidateKeys sk ak gk) = do
  ArbitraryBytes sk' <- shrink $ ArbitraryBytes sk
  ArbitraryBytes ak' <- shrink $ ArbitraryBytes ak
  ArbitraryBytes gk' <- shrink $ ArbitraryBytes gk
  pure $ PermissionedCandidateKeys sk' ak' gk'

shrinkPCPR :: PermissionedCandidatesPolicyRedeemer -> [PermissionedCandidatesPolicyRedeemer]
shrinkPCPR = const []

shrinkPCVD :: PermissionedCandidatesValidatorDatum -> [PermissionedCandidatesValidatorDatum]
shrinkPCVD (PermissionedCandidatesValidatorDatum apck) = PermissionedCandidatesValidatorDatum <$> liftShrink shrinkPCK apck

shrinkPCVR :: PermissionedCandidatesValidatorRedeemer -> [PermissionedCandidatesValidatorRedeemer]
shrinkPCVR = const []

shrinkRD :: ReserveDatum -> [ReserveDatum]
shrinkRD = const []

shrinkRR :: ReserveRedeemer -> [ReserveRedeemer]
shrinkRR = const []

shrinkICSR :: IlliquidCirculationSupplyRedeemer -> [IlliquidCirculationSupplyRedeemer]
shrinkICSR = const []

shrinkMSGP :: MultiSigGovParams -> [MultiSigGovParams]
shrinkMSGP (MultiSigGovParams pkhs a) = do
  pkhs' <- flip liftShrink pkhs $ \pkh -> do
    ArbitraryPubKeyHash pkh' <- shrink (ArbitraryPubKeyHash pkh)
    pure pkh'
  a' <- shrink a
  pure $ MultiSigGovParams pkhs' a'

shrinkVO :: VersionOracle -> [VersionOracle]
shrinkVO (VersionOracle scriptID) = do
  sid <- shrink scriptID
  pure $ VersionOracle sid

shrinkVOC :: VersionOracleConfig -> [VersionOracleConfig]
shrinkVOC (VersionOracleConfig versionOracleCurrencySymbol) = do
  ArbitraryCurrencySymbol sym <- shrink (ArbitraryCurrencySymbol versionOracleCurrencySymbol)
  pure $ VersionOracleConfig sym

shrinkBPRM :: BlockProducerRegistrationMsg -> [BlockProducerRegistrationMsg]
shrinkBPRM (BlockProducerRegistrationMsg {..}) = do
  sp' <- shrinkSP sidechainParams
  EcdsaSecp256k1PubKey spk' <- shrinkPK (EcdsaSecp256k1PubKey sidechainPubKey)
  ArbitraryTxOutRef tout' <- shrink (ArbitraryTxOutRef inputUtxo)
  pure . BlockProducerRegistrationMsg sp' spk' $ tout'

shrinkSO :: StakeOwnership -> [StakeOwnership]
shrinkSO = \case
  AdaBasedStaking pk sig -> do
    ArbitraryPubKey pk' <- shrink (ArbitraryPubKey pk)
    ArbitrarySignature sig' <- shrink (ArbitrarySignature sig)
    pure $ AdaBasedStaking pk' sig'
  TokenBasedStaking -> []

shrinkBPR :: BlockProducerRegistration -> [BlockProducerRegistration]
shrinkBPR (BlockProducerRegistration {..}) = do
  so' <- shrinkSO stakeOwnership
  EcdsaSecp256k1PubKey sidePk' <- shrinkPK (EcdsaSecp256k1PubKey sidechainPubKey)
  EcdsaSecp256k1PubKey auraKey' <- shrinkPK (EcdsaSecp256k1PubKey auraKey)
  EcdsaSecp256k1PubKey grandpaKey' <- shrinkPK (EcdsaSecp256k1PubKey grandpaKey)
  ArbitrarySignature sideSig' <- shrink (ArbitrarySignature sidechainSignature)
  ArbitraryTxOutRef tout' <- shrink (ArbitraryTxOutRef inputUtxo)
  ArbitraryPubKeyHash pkh' <- shrink (ArbitraryPubKeyHash ownPkh)
  pure
    $ BlockProducerRegistration
      so'
      sidePk'
      sideSig'
      tout'
      pkh'
      auraKey'
      grandpaKey'

-- We don't shrink these, as it wouldn't make much sense to
shrinkPK :: EcdsaSecp256k1PubKey -> [EcdsaSecp256k1PubKey]
shrinkPK = const []

shrinkGA :: GovernanceAuthority -> [GovernanceAuthority]
shrinkGA (GovernanceAuthority pkh) = do
  ArbitraryPubKeyHash pkh' <- shrink (ArbitraryPubKeyHash pkh)
  pure $ GovernanceAuthority pkh'

shrinkSP :: SidechainParams -> [SidechainParams]
shrinkSP (SidechainParams {..}) = do
  ArbitraryTxOutRef gu' <- shrink . ArbitraryTxOutRef $ genesisUtxo
  ga <- shrinkGA governanceAuthority
  -- We don't shrink the denominator, as this could make the result _bigger_.
  pure . SidechainParams gu' $ ga

-- | Wrapper for 'PubKey' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryPubKey = ArbitraryPubKey PubKey
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via PubKey
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as this wouldn't make much sense.
--
-- @since v4.0.0
instance Arbitrary ArbitraryPubKey where
  arbitrary =
    ArbitraryPubKey
      . PubKey
      . LedgerBytes
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 64 arbitrary

-- | Wrapper for 'Signature' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitrarySignature = ArbitrarySignature Signature
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via Signature
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as this wouldn't make much sense.
--
-- @since v4.0.0
instance Arbitrary ArbitrarySignature where
  arbitrary =
    ArbitrarySignature
      . Signature
      . LedgerBytes
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 64 arbitrary
