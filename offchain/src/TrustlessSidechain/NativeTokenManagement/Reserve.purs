module TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveValidator
  , reserveAuthPolicy
  , initialiseReserveUtxo
  , findReserveUtxos
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData (Datum(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), Validator)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( ScriptRef(..)
  , TransactionInput
  , TransactionOutputWithRefScript
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( DatumPresence(..)
  , InputWithScriptRef(..)
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , mkTokenName
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
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.NativeTokenManagement.Types
  ( ImmutableReserveSettings
  , MutableReserveSettings
  , ReserveDatum(..)
  , ReserveStats(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
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

reserveVersionOracle ∷ VersionOracle
reserveVersionOracle = VersionOracle
  { version: BigInt.fromInt 1, scriptId: ReserveValidator }

reserveAuthVersionOracle ∷ VersionOracle
reserveAuthVersionOracle =
  VersionOracle
    { version: BigInt.fromInt 1, scriptId: ReserveAuthPolicy }

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
    (governanceRefTxInput /\ governanceRefTxOutput) ←
      getGovernanceScriptRefUtxo sidechainParams

    (reserveRefTxInput /\ reserveRefTxOutput) ←
      getReserveScriptRefUtxo sidechainParams

    reserveAuthCurrencySymbol ← getReserveAuthCurrencySymbol sidechainParams

    governancePolicy ← getGovernancePolicy sidechainParams

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
        Lookups.unspentOutputs
          (Map.singleton reserveRefTxInput reserveRefTxOutput)
          <> Lookups.unspentOutputs
            (Map.singleton governanceRefTxInput governanceRefTxOutput)
          <> Lookups.mintingPolicy reserveAuthPolicy'

      constraints =
        TxConstraints.mustMintValue reserveAuthTokenValue
          <> TxConstraints.mustMintCurrencyUsingScriptRef
            (Scripts.mintingPolicyHash governancePolicy)
            emptyTokenName
            (BigInt.fromInt 1)
            ( RefInput $ mkTxUnspentOut
                governanceRefTxInput
                governanceRefTxOutput
            )
          <> TxConstraints.mustPayToScript
            reserveValidator'
            (Datum $ toData initialReserveDatum)
            DatumInline
            ( Value.singleton tokenKindCs tokenKindTn numOfTokens
                <> reserveAuthTokenValue
            )
          <> TxConstraints.mustReferenceOutput reserveRefTxInput
          <> TxConstraints.mustReferenceOutput governanceRefTxInput

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

