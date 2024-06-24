module TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( findIlliquidCirculationSupplyUtxos
  , illiquidCirculationSupplyValidator
  , depositMoreToSupply
  , withdrawFromSupply
  ) where

import Contract.Prelude

import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Address (Address)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(..))
import Contract.PlutusData (Redeemer(..), RedeemerDatum(..), toData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Cardano.Types.Int as Int
import Cardano.Types.Value as Value
import Contract.Scripts as Scripts
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionInput (TransactionInput)
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap)
import Cardano.Types.Value (Value)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum as BigNum
import Data.Map as Map
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.NativeTokenManagement.Types
  ( IlliquidCirculationSupplyRedeemer(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Utils.Asset (emptyAssetName)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(..)
  , VersionOracleConfig
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

illiquidCirculationSupplyValidator ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) PlutusScript
illiquidCirculationSupplyValidator voc =
  mkValidatorWithParams IlliquidCirculationSupplyValidator [ toData voc ]

icsValidatorVersionOracle ∷ VersionOracle
icsValidatorVersionOracle = VersionOracle
  { version: BigNum.fromInt 1, scriptId: IlliquidCirculationSupplyValidator }

icsWithdrawalTokenName ∷ AssetName
icsWithdrawalTokenName = emptyAssetName

icsWithdrawalPolicyVersionOracle ∷ VersionOracle
icsWithdrawalPolicyVersionOracle =
  VersionOracle
    { version: BigNum.fromInt 1
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
    (TransactionInput /\ TransactionOutput)
getIcsWithdrawalPolicyScriptRefUtxo sidechainParams =
  Versioning.getVersionedScriptRefUtxo
    sidechainParams
    icsWithdrawalPolicyVersionOracle

illiquidCirculationSupplyLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { icsLookups ∷ Lookups.ScriptLookups
    , icsConstraints ∷ TxConstraints.TxConstraints
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
  (TransactionInput /\ TransactionOutput) →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
depositMoreToSupply sp depositedValue utxo = do

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  illiquidCirculationSupplyValidator' ← illiquidCirculationSupplyValidator
    versionOracleConfig

  let
    icsValidatorHash = Scripts.validatorHash illiquidCirculationSupplyValidator'

    value = unwrap >>> _.amount $ snd utxo

  newValue <- fromMaybeThrow
    (GenericInternalError "Couldn't add values.")
    $ pure (value `Value.add` depositedValue)

  let
    lookups ∷ Lookups.ScriptLookups
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
          (RedeemerDatum $ toData DepositMoreToSupply)

  void $ balanceSignAndSubmit
    "Illiquid circulation supply DepositMoreToSupply transaction"
    { constraints, lookups }

withdrawFromSupply ∷
  ∀ r.
  SidechainParams →
  ScriptHash →
  Value →
  (TransactionInput /\ TransactionOutput) →
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

    value = unwrap >>> _.amount $ snd utxo

  newValue <- fromMaybeThrow
    (GenericInternalError "Couldn't subtract values.")
    $ pure (value `Value.minus` withdrawnValue)

  let
    lookups ∷ Lookups.ScriptLookups
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
          (RedeemerDatum $ toData WithdrawFromSupply)
        <> TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          mintingPolicyHash
          unitRedeemer
          icsWithdrawalTokenName
          (Int.fromInt 1)
          ( RefInput $ TransactionUnspentOutput
             { input: withdrawalPolicyInput
             , output: withdrawalPolicyOutput
             }
          )
        <> icsConstraints

  void $ balanceSignAndSubmit
    "Illiquid circulation supply WithdrawFromSupply transaction"
    { constraints, lookups }
