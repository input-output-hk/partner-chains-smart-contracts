module Test.Data
  ( tests
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.PaymentPubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Cardano.Types.Asset (Asset(Asset))
import Contract.Numeric.BigNum as BigNum
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Control.Alt ((<|>))
import Ctl.Internal.Types.Interval (POSIXTime(..))
import Data.Array.NonEmpty as NE
import JS.BigInt as BigInt
import Mote.Monad (test)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, vectorOf)
import Test.QuickCheck.Gen as QGen
import Test.Utils (WrappedTests, pureGroup)
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
  , suchThatMap
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
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.NativeTokenManagement.Types
  ( IlliquidCirculationSupplyRedeemer(..)
  , ImmutableReserveSettings(..)
  , MutableReserveSettings(..)
  , ReserveDatum(..)
  , ReserveRedeemer(..)
  , ReserveStats(..)
  )
import TrustlessSidechain.PermissionedCandidates.Types
  ( PermissionedCandidateKeys(PermissionedCandidateKeys)
  , PermissionedCandidatesPolicyRedeemer
      ( PermissionedCandidatesMint
      , PermissionedCandidatesBurn
      )
  , PermissionedCandidatesValidatorDatum(PermissionedCandidatesValidatorDatum)
  , PermissionedCandidatesValidatorRedeemer
      ( UpdatePermissionedCandidates
      , RemovePermissionedCandidates
      )
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , ecdsaSecp256k1PubKey
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( CommitteeCandidateValidator
      , CandidatePermissionPolicy
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

tests ∷ WrappedTests
tests = pureGroup "Data roundtrip tests" $ do
  test "SidechainParams" $ liftEffect $ toDataLaws testCount genSP
  test "EcdsaSecp256k1PubKey" $ liftEffect $ toDataLaws smallTestCount genPK
  test "BlockProducerRegistration" $ liftEffect $ toDataLaws testCount genBPR
  test "BlockProducerRegistrationMsg" $ liftEffect $ toDataLaws testCount
    genBPRM
  -- BlockProducerRegistrationMsg?
  -- FUELRedeemer not exported
  test "InitTokenAssetClass" $ liftEffect $ toDataLaws testCount
    genInitTokenAssetClass
  test "DParameterValidatorDatum" $ liftEffect $ toDataLaws testCount
    genDParameterValidatorDatum
  test "PermissionedCandidatesValidatorRedeemer" $ liftEffect $ toDataLaws
    testCount
    genPermissionedCandidatesValidatorRedeemer
  test "PermissionedCandidatesValidatorDatum" $ liftEffect $ toDataLaws testCount
    genPermissionedCandidatesValidatorDatum
  test "PermissionedCandidatesPolicyRedeemer" $ liftEffect $ toDataLaws testCount
    genPermissionedCandidatesPolicyRedeemer
  test "PermissionedCandidateKeys" $ liftEffect $ toDataLaws testCount
    genPermissionedCandidateKeys
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
  testCount ∷ Int
  testCount = 10_000

  smallTestCount ∷ Int
  smallTestCount = 1_000

-- Generators

genBPRM ∷ Gen BlockProducerRegistrationMsg
genBPRM = do
  bprmSidechainParams ← genSP
  bprmSidechainPubKey ← genGH
  ArbitraryTransactionInput bprmInputUtxo ← arbitrary
  pure $ BlockProducerRegistrationMsg
    { bprmSidechainParams
    , bprmSidechainPubKey
    , bprmInputUtxo
    }

genInitTokenAssetClass ∷ Gen InitTokenAssetClass
genInitTokenAssetClass = do
  ArbitraryScriptHash initTokenCurrencySymbol ← arbitrary
  ArbitraryAssetName initTokenName ← arbitrary
  pure $ InitTokenAssetClass { initTokenCurrencySymbol, initTokenName }

genDParameterValidatorDatum ∷ Gen DParameterValidatorDatum
genDParameterValidatorDatum = do
  permissionedCandidatesCount ← BigInt.fromInt <$> arbitrary
  registeredCandidatesCount ← BigInt.fromInt <$> arbitrary

  pure $ DParameterValidatorDatum
    { permissionedCandidatesCount
    , registeredCandidatesCount
    }

genPermissionedCandidatesValidatorRedeemer ∷
  Gen PermissionedCandidatesValidatorRedeemer
genPermissionedCandidatesValidatorRedeemer = QGen.oneOf $ NE.cons'
  (pure UpdatePermissionedCandidates)
  [ pure RemovePermissionedCandidates ]

genPermissionedCandidatesValidatorDatum ∷
  Gen PermissionedCandidatesValidatorDatum
genPermissionedCandidatesValidatorDatum = do
  PermissionedCandidatesValidatorDatum <<< { candidates: _ } <$> QGen.arrayOf
    genPermissionedCandidateKeys

genPermissionedCandidatesPolicyRedeemer ∷
  Gen PermissionedCandidatesPolicyRedeemer
genPermissionedCandidatesPolicyRedeemer = QGen.oneOf $ NE.cons'
  (pure PermissionedCandidatesMint)
  [ pure PermissionedCandidatesBurn ]

genPermissionedCandidateKeys ∷ Gen PermissionedCandidateKeys
genPermissionedCandidateKeys = do
  sidechainKey ← arbitrary
  auraKey ← arbitrary
  grandpaKey ← arbitrary

  pure $ PermissionedCandidateKeys
    { sidechainKey
    , auraKey
    , grandpaKey
    }

genScriptId ∷ Gen ScriptId
genScriptId = QGen.oneOf $ NE.cons' (pure CommitteeCandidateValidator) $ pure
  <$>
    [ CandidatePermissionPolicy
    , VersionOraclePolicy
    , VersionOracleValidator
    , DParameterPolicy
    , DParameterValidator
    , PermissionedCandidatesPolicy
    , PermissionedCandidatesValidator
    , ScriptCache
    ]

genVersionOracle ∷ Gen VersionOracle
genVersionOracle = do
  version ← BigNum.fromInt <$> arbitrary
  scriptId ← genScriptId
  pure $ VersionOracle
    { version
    , scriptId
    }

genVersionOracleConfig ∷ Gen VersionOracleConfig
genVersionOracleConfig = do
  ArbitraryScriptHash versionOracleCurrencySymbol ← arbitrary
  pure $ VersionOracleConfig
    { versionOracleCurrencySymbol
    }

genVersionOraclePolicyRedeemer ∷ Gen VersionOraclePolicyRedeemer
genVersionOraclePolicyRedeemer = QGen.oneOf $ NE.cons'
  ( do
      versionOracle ← genVersionOracle
      ArbitraryScriptHash scriptHash ← arbitrary
      pure $ InitializeVersionOracle versionOracle scriptHash
  )
  [ do
      versionOracle ← genVersionOracle
      ArbitraryScriptHash scriptHash ← arbitrary
      pure $ MintVersionOracle versionOracle scriptHash
  , BurnVersionOracle <$> genVersionOracle
  ]

genReserveDatum ∷ Gen ReserveDatum
genReserveDatum = do
  ArbitraryBigInt pt ← arbitrary
  ArbitraryScriptHash cs ← arbitrary
  c ← arbitrary
  ArbitraryScriptHash sh ← arbitrary
  ArbitraryAssetName an ← arbitrary
  i ← arbitrary

  pure $
    ReserveDatum
      { immutableSettings: ImmutableReserveSettings
          { t0: (POSIXTime pt), tokenKind: Asset sh an }
      , mutableSettings: MutableReserveSettings
          { vFunctionTotalAccrued: cs, incentiveAmount: BigInt.fromInt i }
      , stats: ReserveStats { tokenTotalAmountTransferred: BigInt.fromInt c }
      }

genReserveRedeemer ∷ Gen ReserveRedeemer
genReserveRedeemer = do
  ArbitraryBigInt governanceVersion ← arbitrary
  QGen.oneOf $ NE.cons'
    ( pure $ DepositToReserve { governanceVersion }
    )
    [ pure TransferToIlliquidCirculationSupply
    , pure $ UpdateReserve { governanceVersion }
    , pure $ Handover { governanceVersion }
    ]

genIlliquidCirculationSupplyRedeemer ∷ Gen IlliquidCirculationSupplyRedeemer
genIlliquidCirculationSupplyRedeemer = QGen.oneOf $ NE.cons'
  ( pure DepositMoreToSupply
  )
  [ pure WithdrawFromSupply
  ]

genGA ∷ Gen GovernanceAuthority
genGA = do
  ArbitraryPaymentPubKeyHash (PaymentPubKeyHash pkh) ← arbitrary
  pure $ GovernanceAuthority (wrap $ unwrap pkh)

genSO ∷ Gen StakeOwnership
genSO =
  ( ado
      ArbitraryPubKey pk ← arbitrary
      ArbitrarySignature sig ← arbitrary
      in AdaBasedStaking pk sig
  )
    <|> pure TokenBasedStaking

genBPR ∷ Gen BlockProducerRegistration
genBPR = do
  stakeOwnership ← genSO
  sidechainPubKey ← genGH
  auraKey ← genGH
  grandpaKey ← genGH
  sidechainSignature ← genGH
  ArbitraryTransactionInput inputUtxo ← arbitrary
  ArbitraryPaymentPubKeyHash (PaymentPubKeyHash ownPkh) ← arbitrary
  pure $ BlockProducerRegistration
    { stakeOwnership
    , sidechainPubKey
    , sidechainSignature
    , inputUtxo
    , ownPkh: (wrap $ unwrap ownPkh)
    , auraKey
    , grandpaKey
    }

genPK ∷ Gen EcdsaSecp256k1PubKey
genPK = suchThatMap (genByteArrayLen 33) ecdsaSecp256k1PubKey

genSP ∷ Gen SidechainParams
genSP = do
  NonNegative (ArbitraryBigInt chainId) ← arbitrary
  ArbitraryTransactionInput genesisUtxo ← arbitrary
  Positive (ArbitraryBigInt thresholdNumerator) ← arbitrary
  Positive (ArbitraryBigInt thresholdDenominator) ← arbitrary
  governanceAuthority ← genGA
  pure $ SidechainParams
    { chainId
    , genesisUtxo
    , thresholdNumerator
    , thresholdDenominator
    , governanceAuthority
    }

genGH ∷ Gen ByteArray
genGH = byteArrayFromIntArrayUnsafe <$> arrayOf (chooseInt 0 255)

genByteArrayLen ∷ Int → Gen ByteArray
genByteArrayLen len =
  byteArrayFromIntArrayUnsafe <$> vectorOf len (chooseInt 0 255)
