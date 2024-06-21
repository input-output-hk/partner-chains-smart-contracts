module TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( findIlliquidCirculationSupplyUtxos
  , illiquidCirculationSupplyValidator
  , depositMoreToSupply
  , withdrawFromSupply
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData (Redeemer(..), toData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicyHash, Validator)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap)
import Contract.Value (TokenName, Value, mkTokenName, negation)
import Data.BigInt as BigInt
import Data.Map as Map
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.NativeTokenManagement.Types
  ( IlliquidCirculationSupplyRedeemer(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(..)
  , VersionOracleConfig
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

illiquidCirculationSupplyValidator ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) Validator
illiquidCirculationSupplyValidator voc =
  mkValidatorWithParams IlliquidCirculationSupplyValidator [ toData voc ]

icsValidatorVersionOracle ∷ VersionOracle
icsValidatorVersionOracle = VersionOracle
  { version: BigInt.fromInt 1, scriptId: IlliquidCirculationSupplyValidator }

emptyTokenName ∷ TokenName
emptyTokenName = unsafePartial $ fromJust $ mkTokenName mempty

icsWithdrawalTokenName ∷ TokenName
icsWithdrawalTokenName = emptyTokenName

icsWithdrawalPolicyVersionOracle ∷ VersionOracle
icsWithdrawalPolicyVersionOracle =
  VersionOracle
    { version: BigInt.fromInt 1
    , scriptId: IlliquidCirculationSupplyWithdrawalPolicy
    }

getIlliquidCirculationSupplyAddress ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    Address
getIlliquidCirculationSupplyAddress sidechainParams =
  Versioning.getVersionedValidatorAddress
    sidechainParams
    icsValidatorVersionOracle

findIlliquidCirculationSupplyUtxos ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    UtxoMap
findIlliquidCirculationSupplyUtxos sidechainParams =
  getIlliquidCirculationSupplyAddress sidechainParams >>= utxosAt

getIcsWithdrawalPolicyScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
getIcsWithdrawalPolicyScriptRefUtxo sidechainParams =
  Versioning.getVersionedScriptRefUtxo
    sidechainParams
    icsWithdrawalPolicyVersionOracle

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
    getIcsWithdrawalPolicyScriptRefUtxo sp

  pure
    { icsLookups: Lookups.unspentOutputs
        (Map.singleton icsRefTxInput icsRefTxOutput)
    , icsConstraints: TxConstraints.mustReferenceOutput
        icsRefTxInput
    }

depositMoreToSupply ∷
  ∀ r.
  SidechainParams →
  Value →
  (TransactionInput /\ TransactionOutputWithRefScript) →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
depositMoreToSupply sp depositedValue utxo = do

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  illiquidCirculationSupplyValidator' ← illiquidCirculationSupplyValidator
    versionOracleConfig

  let
    icsValidatorHash = Scripts.validatorHash illiquidCirculationSupplyValidator'

    value = unwrap >>> _.output >>> unwrap >>> _.amount $ snd utxo

    newValue = value <> depositedValue

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator illiquidCirculationSupplyValidator'

    constraints =
      TxConstraints.mustPayToScript
        icsValidatorHash
        unitDatum
        DatumInline
        newValue
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (toData >>> Redeemer $ DepositMoreToSupply)

  void $ balanceSignAndSubmit
    "Illiquid circulation supply DepositMoreToSupply transaction"
    { constraints, lookups }

withdrawFromSupply ∷
  ∀ r.
  SidechainParams →
  MintingPolicyHash →
  Value →
  (TransactionInput /\ TransactionOutputWithRefScript) →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
withdrawFromSupply sp mintingPolicyHash withdrawnValue utxo = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp

  { icsLookups
  , icsConstraints
  } ← illiquidCirculationSupplyLookupsAndConstraints sp

  illiquidCirculationSupplyValidator' ← illiquidCirculationSupplyValidator
    versionOracleConfig

  (withdrawalPolicyInput /\ withdrawalPolicyOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sp
      icsWithdrawalPolicyVersionOracle
  let
    icsValidatorHash = Scripts.validatorHash illiquidCirculationSupplyValidator'

    value = unwrap >>> _.output >>> unwrap >>> _.amount $ snd utxo

    newValue = value <> negation withdrawnValue

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator illiquidCirculationSupplyValidator'
        <> icsLookups

    constraints =
      TxConstraints.mustPayToScript
        icsValidatorHash
        unitDatum
        DatumInline
        newValue
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (toData >>> Redeemer $ WithdrawFromSupply)
        <> TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          mintingPolicyHash
          unitRedeemer
          icsWithdrawalTokenName
          one
          ( RefInput $ mkTxUnspentOut withdrawalPolicyInput
              withdrawalPolicyOutput
          )
        <> icsConstraints

  void $ balanceSignAndSubmit
    "Illiquid circulation supply WithdrawFromSupply transaction"
    { constraints, lookups }
