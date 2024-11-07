module Test.Data
  ( tests
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.PaymentPubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Cardano.Types.Asset (Asset(Asset))
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Control.Alt ((<|>))
import Ctl.Internal.Types.Interval (POSIXTime(..))
import Data.Array.NonEmpty as NE
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt)
import Test.QuickCheck.Gen as QGen
import Test.Utils (PureTest)
import Test.Utils.Laws (toDataLaws)
import Test.Utils.QuickCheck
  ( ArbitraryAssetName(ArbitraryAssetName)
  , ArbitraryBigInt(ArbitraryBigInt)
  , ArbitraryPaymentPubKeyHash(ArbitraryPaymentPubKeyHash)
  , ArbitraryPubKey(ArbitraryPubKey)
  , ArbitraryScriptHash(ArbitraryScriptHash)
  , ArbitrarySignature(ArbitrarySignature)
  , ArbitraryTransactionInput(ArbitraryTransactionInput)
  , NonNegative(NonNegative)
  , Positive(Positive)
  )
import TrustlessSidechain.CommitteeCandidateValidator
  ( BlockProducerRegistration(BlockProducerRegistration)
  , BlockProducerRegistrationMsg(BlockProducerRegistrationMsg)
  , StakeOwnership(AdaBasedStaking, TokenBasedStaking)
  )
import TrustlessSidechain.DParameter.Types
  ( DParameterValidatorDatum(DParameterValidatorDatum)
  )
import TrustlessSidechain.Governance.Admin
  ( GovernanceAuthority(GovernanceAuthority)
  )
import TrustlessSidechain.NativeTokenManagement.Types
  ( IlliquidCirculationSupplyRedeemer(..)
  , ImmutableReserveSettings(..)
  , MutableReserveSettings(..)
  , ReserveDatum(..)
  , ReserveRedeemer(..)
  , ReserveStats(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( CommitteeCandidateValidator
      , VersionOraclePolicy
      , VersionOracleValidator
      , DParameterPolicy
      , DParameterValidator
      , PermissionedCandidatesPolicy
      , PermissionedCandidatesValidator
      , ScriptCache
      )
  , VersionOracle(VersionOracle)
  , VersionOracleConfig(VersionOracleConfig)
  , VersionOraclePolicyRedeemer
      ( InitializeVersionOracle
      , MintVersionOracle
      , BurnVersionOracle
      )
  )

tests :: PureTest
tests = group "Data roundtrip tests" $ do
  test "SidechainParams" $ liftEffect $ toDataLaws testCount genSP
  test "BlockProducerRegistration" $ liftEffect $ toDataLaws testCount genBPR
  test "BlockProducerRegistrationMsg" $ liftEffect $ toDataLaws testCount
    genBPRM
  -- BlockProducerRegistrationMsg?
  -- FUELRedeemer not exported
  test "DParameterValidatorDatum" $ liftEffect $ toDataLaws testCount
    genDParameterValidatorDatum
  test "ScriptId" $ liftEffect $ toDataLaws testCount genScriptId
  test "VersionOracle" $ liftEffect $ toDataLaws testCount genVersionOracle
  test "VersionOracleConfig" $ liftEffect $ toDataLaws testCount
    genVersionOracleConfig
  test "VersionOraclePolicyRedeemer" $ liftEffect $ toDataLaws testCount
    genVersionOraclePolicyRedeemer
  test "ReserveDatum" $ liftEffect $ toDataLaws testCount
    genReserveDatum
  test "ReserveRedeemer" $ liftEffect $ toDataLaws testCount
    genReserveRedeemer
  test "IlliquidCirculationSupplyRedeemer" $ liftEffect $ toDataLaws testCount
    genIlliquidCirculationSupplyRedeemer
  where
  testCount :: Int
  testCount = 10_000

-- Generators

genBPRM :: Gen BlockProducerRegistrationMsg
genBPRM = do
  bprmSidechainParams <- genSP
  bprmSidechainPubKey <- genGH
  ArbitraryTransactionInput bprmInputUtxo <- arbitrary
  pure $ BlockProducerRegistrationMsg
    { bprmSidechainParams
    , bprmSidechainPubKey
    , bprmInputUtxo
    }

genDParameterValidatorDatum :: Gen DParameterValidatorDatum
genDParameterValidatorDatum = do
  permissionedCandidatesCount <- BigInt.fromInt <$> arbitrary
  registeredCandidatesCount <- BigInt.fromInt <$> arbitrary

  pure $ DParameterValidatorDatum
    { permissionedCandidatesCount
    , registeredCandidatesCount
    }

genScriptId :: Gen ScriptId
genScriptId = QGen.oneOf $ NE.cons' (pure CommitteeCandidateValidator) $ pure
  <$>
    [ VersionOraclePolicy
    , VersionOracleValidator
    , DParameterPolicy
    , DParameterValidator
    , PermissionedCandidatesPolicy
    , PermissionedCandidatesValidator
    , ScriptCache
    ]

genVersionOracle :: Gen VersionOracle
genVersionOracle = do
  scriptId <- genScriptId
  pure $ VersionOracle
    { scriptId
    }

genVersionOracleConfig :: Gen VersionOracleConfig
genVersionOracleConfig = do
  ArbitraryScriptHash versionOracleCurrencySymbol <- arbitrary
  pure $ VersionOracleConfig
    { versionOracleCurrencySymbol
    }

genVersionOraclePolicyRedeemer :: Gen VersionOraclePolicyRedeemer
genVersionOraclePolicyRedeemer = QGen.oneOf $ NE.cons'
  ( do
      versionOracle <- genVersionOracle
      ArbitraryScriptHash scriptHash <- arbitrary
      pure $ InitializeVersionOracle versionOracle scriptHash
  )
  [ do
      versionOracle <- genVersionOracle
      ArbitraryScriptHash scriptHash <- arbitrary
      pure $ MintVersionOracle versionOracle scriptHash
  , BurnVersionOracle <$> genVersionOracle
  ]

genReserveDatum :: Gen ReserveDatum
genReserveDatum = do
  ArbitraryBigInt pt <- arbitrary
  ArbitraryScriptHash cs <- arbitrary
  c <- arbitrary
  ArbitraryScriptHash sh <- arbitrary
  ArbitraryAssetName an <- arbitrary
  i <- arbitrary

  pure $
    ReserveDatum
      { immutableSettings: ImmutableReserveSettings
          { t0: (POSIXTime pt), tokenKind: Asset sh an }
      , mutableSettings: MutableReserveSettings
          { vFunctionTotalAccrued: cs, incentiveAmount: BigInt.fromInt i }
      , stats: ReserveStats { tokenTotalAmountTransferred: BigInt.fromInt c }
      }

genReserveRedeemer :: Gen ReserveRedeemer
genReserveRedeemer = do
  ArbitraryBigInt governanceVersion <- arbitrary
  QGen.oneOf $ NE.cons'
    ( pure $ DepositToReserve { governanceVersion }
    )
    [ pure TransferToIlliquidCirculationSupply
    , pure $ UpdateReserve { governanceVersion }
    , pure $ Handover { governanceVersion }
    ]

genIlliquidCirculationSupplyRedeemer :: Gen IlliquidCirculationSupplyRedeemer
genIlliquidCirculationSupplyRedeemer = QGen.oneOf $ NE.cons'
  ( pure DepositMoreToSupply
  )
  [ pure WithdrawFromSupply
  ]

genGA :: Gen GovernanceAuthority
genGA = do
  ArbitraryPaymentPubKeyHash (PaymentPubKeyHash pkh) <- arbitrary
  pure $ GovernanceAuthority (wrap $ unwrap pkh)

genSO :: Gen StakeOwnership
genSO =
  ( ado
      ArbitraryPubKey pk <- arbitrary
      ArbitrarySignature sig <- arbitrary
      in AdaBasedStaking pk sig
  )
    <|> pure TokenBasedStaking

genBPR :: Gen BlockProducerRegistration
genBPR = do
  stakeOwnership <- genSO
  sidechainPubKey <- genGH
  auraKey <- genGH
  grandpaKey <- genGH
  sidechainSignature <- genGH
  ArbitraryTransactionInput inputUtxo <- arbitrary
  pure $ BlockProducerRegistration
    { stakeOwnership
    , sidechainPubKey
    , sidechainSignature
    , inputUtxo
    , auraKey
    , grandpaKey
    }

genSP :: Gen SidechainParams
genSP = do
  NonNegative (ArbitraryBigInt chainId) <- arbitrary
  ArbitraryTransactionInput genesisUtxo <- arbitrary
  Positive (ArbitraryBigInt thresholdNumerator) <- arbitrary
  Positive (ArbitraryBigInt thresholdDenominator) <- arbitrary
  governanceAuthority <- genGA
  pure $ SidechainParams
    { chainId
    , genesisUtxo
    , thresholdNumerator
    , thresholdDenominator
    , governanceAuthority
    }

genGH :: Gen ByteArray
genGH = byteArrayFromIntArrayUnsafe <$> arrayOf (chooseInt 0 255)
