module TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveValidator
  , reserveAuthPolicy
  , initialiseReserveUtxo
  , findReserveUtxos
  , depositToReserve
  , extractReserveDatum
  , updateReserveUtxo
  , transferToIlliquidCirculationSupply
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData
  ( Datum(..)
  , Redeemer(..)
  , fromData
  , toData
  , unitDatum
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), Validator(..))
import Contract.Scripts as Scripts
import Contract.Transaction
  ( ScriptRef(..)
  , TransactionInput
  , TransactionOutput
  , TransactionOutputWithRefScript
  , mkTxUnspentOut
  , outputDatumDatum
  )
import Contract.TxConstraints (DatumPresence(..), InputWithScriptRef(..))
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , mkTokenName
  , mpsSymbol
  , negation
  , valueOf
  )
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.NativeTokenManagement.Types
  ( ImmutableReserveSettings
  , MutableReserveSettings
  , ReserveDatum(..)
  , ReserveRedeemer(..)
  , ReserveStats(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (AssetClass)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(..)
  , VersionOracleConfig
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

reserveValidator ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) Validator
reserveValidator voc =
  mkValidatorWithParams ReserveValidator [ toData voc ]

reserveAuthPolicy ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) MintingPolicy
reserveAuthPolicy voc =
  mkMintingPolicyWithParams ReserveAuthPolicy [ toData voc ]

emptyTokenName ∷ TokenName
emptyTokenName = unsafePartial $ fromJust $ mkTokenName mempty

reserveAuthTokenName ∷ TokenName
reserveAuthTokenName = emptyTokenName

vFunctionTotalAccruedTokenName ∷ TokenName
vFunctionTotalAccruedTokenName = emptyTokenName

reserveVersionOracle ∷ VersionOracle
reserveVersionOracle = VersionOracle
  { version: BigInt.fromInt 1, scriptId: ReserveValidator }

reserveAuthVersionOracle ∷ VersionOracle
reserveAuthVersionOracle =
  VersionOracle
    { version: BigInt.fromInt 1, scriptId: ReserveAuthPolicy }

illiquidCirculationSupplyVersionOracle ∷ VersionOracle
illiquidCirculationSupplyVersionOracle =
  VersionOracle
    { version: BigInt.fromInt 1, scriptId: IlliquidCirculationSupplyValidator }

governanceVersionOracle ∷ VersionOracle
governanceVersionOracle = VersionOracle
  { version: BigInt.fromInt 1, scriptId: GovernancePolicy }

getReserveAuthCurrencySymbol ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    CurrencySymbol
getReserveAuthCurrencySymbol sidechainParams =
  Versioning.getVersionedCurrencySymbol
    sidechainParams
    reserveAuthVersionOracle

getReserveAddress ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    Address
getReserveAddress sidechainParams =
  Versioning.getVersionedValidatorAddress
    sidechainParams
    reserveVersionOracle

getGovernanceScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
getGovernanceScriptRefUtxo sidechainParams =
  Versioning.getVersionedScriptRefUtxo
    sidechainParams
    governanceVersionOracle

getReserveScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
getReserveScriptRefUtxo sidechainParams =
  Versioning.getVersionedScriptRefUtxo
    sidechainParams
    reserveVersionOracle

getReserveAuthScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
getReserveAuthScriptRefUtxo sidechainParams =
  Versioning.getVersionedScriptRefUtxo
    sidechainParams
    reserveAuthVersionOracle

getIlliquidCirculationSupplyScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
getIlliquidCirculationSupplyScriptRefUtxo sidechainParams =
  Versioning.getVersionedScriptRefUtxo
    sidechainParams
    illiquidCirculationSupplyVersionOracle

getGovernancePolicy ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    MintingPolicy
getGovernancePolicy sidechainParams = do
  (_ /\ refTxOutput) ← getGovernanceScriptRefUtxo sidechainParams

  case (unwrap refTxOutput).scriptRef of
    Just (PlutusScriptRef s) → pure $ PlutusMintingPolicy s
    _ → throw $ GenericInternalError
      "Versioning system utxo does not carry governance script"

getIlliquidCirculationSupplyValidator ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    Validator
getIlliquidCirculationSupplyValidator sidechainParams = do
  (_ /\ refTxOutput) ← getIlliquidCirculationSupplyScriptRefUtxo sidechainParams

  case (unwrap refTxOutput).scriptRef of
    Just (PlutusScriptRef s) → pure $ Validator s
    _ → throw $ GenericInternalError
      "Versioning system utxo does not carry ICS script"

findReserveUtxos ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    UtxoMap
findReserveUtxos sidechainParams = do
  reserveAuthCurrencySymbol ← getReserveAuthCurrencySymbol sidechainParams

  reserveAddress ← getReserveAddress sidechainParams

  utxos ← utxosAt reserveAddress

  pure $ flip Map.filter utxos $ \o → one ==
    valueOf (unwrap >>> _.output >>> unwrap >>> _.amount $ o)
      reserveAuthCurrencySymbol
      reserveAuthTokenName

reserveAuthLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { reserveAuthLookups ∷ Lookups.ScriptLookups Void
    , reserveAuthConstraints ∷ TxConstraints.TxConstraints Void Void
    }
reserveAuthLookupsAndConstraints sp = do
  (reserveAuthRefTxInput /\ reserveAuthRefTxOutput) ←
    getReserveAuthScriptRefUtxo sp

  pure
    { reserveAuthLookups: Lookups.unspentOutputs
        (Map.singleton reserveAuthRefTxInput reserveAuthRefTxOutput)
    , reserveAuthConstraints: TxConstraints.mustReferenceOutput
        reserveAuthRefTxInput
    }

illiquidCirculationSupplyLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { icsLookups ∷ Lookups.ScriptLookups Void
    , icsConstraints ∷ TxConstraints.TxConstraints Void Void
    }
illiquidCirculationSupplyLookupsAndConstraints sp = do
  (icsRefTxInput /\ icsRefTxOutput) ←
    getIlliquidCirculationSupplyScriptRefUtxo sp

  pure
    { icsLookups: Lookups.unspentOutputs
        (Map.singleton icsRefTxInput icsRefTxOutput)
    , icsConstraints: TxConstraints.mustReferenceOutput
        icsRefTxInput
    }

reserveLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { reserveLookups ∷ Lookups.ScriptLookups Void
    , reserveConstraints ∷ TxConstraints.TxConstraints Void Void
    }
reserveLookupsAndConstraints sp = do
  (reserveRefTxInput /\ reserveRefTxOutput) ←
    getReserveScriptRefUtxo sp

  pure
    { reserveLookups: Lookups.unspentOutputs
        (Map.singleton reserveRefTxInput reserveRefTxOutput)
    , reserveConstraints: TxConstraints.mustReferenceOutput
        reserveRefTxInput
    }

governanceLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { governanceLookups ∷ Lookups.ScriptLookups Void
    , governanceConstraints ∷ TxConstraints.TxConstraints Void Void
    }
governanceLookupsAndConstraints sp = do
  (governanceRefTxInput /\ governanceRefTxOutput) ←
    getGovernanceScriptRefUtxo sp

  governancePolicy ← getGovernancePolicy sp

  pure
    { governanceLookups: Lookups.unspentOutputs
        (Map.singleton governanceRefTxInput governanceRefTxOutput)
    , governanceConstraints:
        TxConstraints.mustReferenceOutput governanceRefTxInput
          <> TxConstraints.mustMintCurrencyUsingScriptRef
            (Scripts.mintingPolicyHash governancePolicy)
            emptyTokenName
            (BigInt.fromInt 1)
            ( RefInput $ mkTxUnspentOut
                governanceRefTxInput
                governanceRefTxOutput
            )
    }

initialiseReserveUtxo ∷
  ∀ r.
  SidechainParams →
  ImmutableReserveSettings →
  MutableReserveSettings →
  BigInt →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
initialiseReserveUtxo
  sidechainParams
  immutableSettings
  mutableSettings
  numOfTokens =
  do
    { governanceLookups
    , governanceConstraints
    } ← governanceLookupsAndConstraints sidechainParams

    { reserveLookups
    , reserveConstraints
    } ← reserveLookupsAndConstraints sidechainParams

    reserveAuthCurrencySymbol ← getReserveAuthCurrencySymbol sidechainParams

    versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams

    reserveValidator' ← Scripts.validatorHash <$> reserveValidator
      versionOracleConfig
    reserveAuthPolicy' ← reserveAuthPolicy versionOracleConfig

    let
      (tokenKindCs /\ tokenKindTn) = (unwrap immutableSettings).tokenKind

      reserveAuthTokenValue =
        Value.singleton
          reserveAuthCurrencySymbol
          reserveAuthTokenName
          (BigInt.fromInt 1)

    let
      lookups ∷ Lookups.ScriptLookups Void
      lookups =
        governanceLookups
          <> reserveLookups
          <> Lookups.mintingPolicy reserveAuthPolicy'

      constraints =
        governanceConstraints
          <> reserveConstraints
          <> TxConstraints.mustMintValue reserveAuthTokenValue
          <> TxConstraints.mustPayToScript
            reserveValidator'
            (Datum $ toData initialReserveDatum)
            DatumInline
            ( Value.singleton tokenKindCs tokenKindTn numOfTokens
                <> reserveAuthTokenValue
            )

    void $ balanceSignAndSubmit
      "Reserve initialization transaction"
      { constraints, lookups }

  where
  initialReserveDatum ∷ ReserveDatum
  initialReserveDatum = ReserveDatum
    { immutableSettings
    , mutableSettings
    , stats: ReserveStats { tokenTotalAmountTransferred: zero }
    }

extractReserveDatum ∷ TransactionOutput → Maybe ReserveDatum
extractReserveDatum txOut =
  outputDatumDatum (unwrap txOut).datum >>= unwrap >>> fromData

findReserveUtxoForAssetClass ∷
  ∀ r.
  SidechainParams →
  AssetClass →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    UtxoMap
findReserveUtxoForAssetClass sp ac = do
  utxos ← findReserveUtxos sp

  let
    extractTokenKind =
      unwrap >>> _.immutableSettings >>> unwrap >>> _.tokenKind
    extractTxOut = unwrap >>> _.output

  pure $ flip Map.filter utxos $ extractTxOut >>> \txOut →
    flip (maybe false) (extractReserveDatum txOut)
      $ extractTokenKind
      >>> (_ == ac)

depositToReserve ∷
  ∀ r.
  SidechainParams →
  AssetClass →
  BigInt →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
depositToReserve sp ac amount = do
  utxo ← fromMaybeThrow (NotFoundUtxo "Reserve UTxO for asset class not found")
    $ (Map.toUnfoldable <$> findReserveUtxoForAssetClass sp ac)

  { governanceLookups
  , governanceConstraints
  } ← governanceLookupsAndConstraints sp

  { reserveAuthLookups
  , reserveAuthConstraints
  } ← reserveAuthLookupsAndConstraints sp

  { icsLookups
  , icsConstraints
  } ← illiquidCirculationSupplyLookupsAndConstraints sp

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  reserveValidator' ← reserveValidator versionOracleConfig

  datum ← fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ outputDatumDatum
    $ unwrap
    >>> _.output
    >>> unwrap
    >>> _.datum
    $ snd utxo

  let
    value = unwrap >>> _.output >>> unwrap >>> _.amount $ snd utxo

    newValue = value <> Value.singleton (fst ac) (snd ac) amount

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      reserveAuthLookups
        <> icsLookups
        <> governanceLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator reserveValidator'

    constraints =
      governanceConstraints
        <> icsConstraints
        <> reserveAuthConstraints
        <> TxConstraints.mustPayToScript
          (Scripts.validatorHash reserveValidator')
          datum
          DatumInline
          newValue
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (toData >>> Redeemer $ DepositToReserve)

  void $ balanceSignAndSubmit
    "Deposit to a reserve utxo"
    { constraints, lookups }

-- utxo passed to this function must be a reserve utxo
-- use `findReserveUtxos` and `extractReserveDatum` to find utxos of interest
updateReserveUtxo ∷
  ∀ r.
  SidechainParams →
  MutableReserveSettings →
  (TransactionInput /\ TransactionOutputWithRefScript) →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
updateReserveUtxo sp updatedMutableSettings utxo = do
  { governanceLookups
  , governanceConstraints
  } ← governanceLookupsAndConstraints sp

  { reserveAuthLookups
  , reserveAuthConstraints
  } ← reserveAuthLookupsAndConstraints sp

  { icsLookups
  , icsConstraints
  } ← illiquidCirculationSupplyLookupsAndConstraints sp

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  reserveValidator' ← reserveValidator versionOracleConfig

  datum ← fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ extractReserveDatum
    $ (snd >>> unwrap >>> _.output)
    $ utxo

  let
    updatedDatum = ReserveDatum $ (unwrap datum)
      { mutableSettings = updatedMutableSettings }
    value = unwrap >>> _.output >>> unwrap >>> _.amount $ snd utxo

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      reserveAuthLookups
        <> icsLookups
        <> governanceLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator reserveValidator'

    constraints =
      governanceConstraints
        <> icsConstraints
        <> reserveAuthConstraints
        <> TxConstraints.mustPayToScript
          (Scripts.validatorHash reserveValidator')
          (Datum $ toData updatedDatum)
          DatumInline
          value
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (toData >>> Redeemer $ UpdateReserve)

  void $ balanceSignAndSubmit
    "Update reserve mutable settings"
    { constraints, lookups }

transferToIlliquidCirculationSupply ∷
  ∀ r.
  SidechainParams →
  BigInt →
  MintingPolicy →
  (TransactionInput /\ TransactionOutputWithRefScript) →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
transferToIlliquidCirculationSupply
  sp
  totalAccruedTillNow
  vFunctionTotalAccruedMintingPolicy
  utxo = do
  { reserveAuthLookups
  , reserveAuthConstraints
  } ← reserveAuthLookupsAndConstraints sp

  { icsLookups
  , icsConstraints
  } ← illiquidCirculationSupplyLookupsAndConstraints sp

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  reserveValidator' ← reserveValidator versionOracleConfig

  illiquidCirculationSupplyValidator ← getIlliquidCirculationSupplyValidator sp

  datum ← fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ extractReserveDatum
    $ (snd >>> unwrap >>> _.output)
    $ utxo

  let
    tokenKind =
      unwrap
        >>> _.immutableSettings
        >>> unwrap
        >>> _.tokenKind
        $ datum

    tokenTotalAmountTransferred =
      unwrap
        >>> _.stats
        >>> unwrap
        >>> _.tokenTotalAmountTransferred
        $ datum

    vFunctionTotalAccruedCurrencySymbol =
      unwrap
        >>> _.mutableSettings
        >>> unwrap
        >>> _.vFunctionTotalAccrued
        $ datum

  unless
    ( mpsSymbol (Scripts.mintingPolicyHash vFunctionTotalAccruedMintingPolicy) ==
        Just vFunctionTotalAccruedCurrencySymbol
    ) $ throw (InvalidData "Passed ICS minting policy is not correct")

  let
    toTransferAsInt = totalAccruedTillNow - tokenTotalAmountTransferred

    toTransferAsValue =
      Value.singleton (fst tokenKind) (snd tokenKind) toTransferAsInt

    vtTokensAsValue = Value.singleton
      vFunctionTotalAccruedCurrencySymbol
      vFunctionTotalAccruedTokenName
      toTransferAsInt

    updatedDatum = ReserveDatum $ (unwrap datum)
      { stats = ReserveStats { tokenTotalAmountTransferred: totalAccruedTillNow }
      }
    value = unwrap >>> _.output >>> unwrap >>> _.amount $ snd utxo
    newValue = value <> negation toTransferAsValue

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      reserveAuthLookups
        <> icsLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator reserveValidator'
        <> Lookups.mintingPolicy vFunctionTotalAccruedMintingPolicy

    constraints =
      reserveAuthConstraints
        <> icsConstraints
        <> TxConstraints.mustPayToScript
          (Scripts.validatorHash reserveValidator')
          (Datum $ toData updatedDatum)
          DatumInline
          newValue
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (toData >>> Redeemer $ TransferToIlliquidCirculationSupply)
        <> TxConstraints.mustMintValue vtTokensAsValue
        <> TxConstraints.mustPayToScript
          (Scripts.validatorHash illiquidCirculationSupplyValidator)
          unitDatum
          DatumInline
          toTransferAsValue

  void $ balanceSignAndSubmit
    "Transfer to illiquid circulation supply"
    { constraints, lookups }
