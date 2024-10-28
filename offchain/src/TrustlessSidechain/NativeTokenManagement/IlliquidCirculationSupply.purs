module TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( illiquidCirculationSupplyValidator
  , depositMoreToSupply
  , withdrawFromSupply
  ) where

import Contract.Prelude

import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.PlutusData (unit) as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(..))
import Cardano.Types.Value (Value)
import Cardano.Types.Value as Value
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as TxConstraints
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.NativeTokenManagement.Types
  ( IlliquidCirculationSupplyRedeemer(DepositMoreToSupply, WithdrawFromSupply)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Asset (emptyAssetName)
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(VersionOracle)
  , VersionOracleConfig
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

illiquidCirculationSupplyValidator ::
  forall r.
  VersionOracleConfig ->
  Run (EXCEPT OffchainError + r) PlutusScript
illiquidCirculationSupplyValidator voc =
  mkValidatorWithParams IlliquidCirculationSupplyValidator [ toData voc ]

icsWithdrawalTokenName :: AssetName
icsWithdrawalTokenName = emptyAssetName

illiquidCirculationSupplyLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { icsLookups :: Lookups.ScriptLookups
    , icsConstraints :: TxConstraints.TxConstraints
    }
illiquidCirculationSupplyLookupsAndConstraints sp = do
  (icsRefTxInput /\ icsRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo sp
      ( VersionOracle
          { scriptId: IlliquidCirculationSupplyWithdrawalPolicy
          }
      )

  pure
    { icsLookups: Lookups.unspentOutputs
        (Map.singleton icsRefTxInput icsRefTxOutput)
    , icsConstraints: TxConstraints.mustReferenceOutput
        icsRefTxInput
    }

depositMoreToSupply ::
  forall r.
  SidechainParams ->
  Value ->
  (TransactionInput /\ TransactionOutput) ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
depositMoreToSupply sp depositedValue utxo = do

  versionOracleConfig <- Versioning.getVersionOracleConfig sp
  illiquidCirculationSupplyValidator' <- illiquidCirculationSupplyValidator
    versionOracleConfig

  let
    icsValidatorHash = PlutusScript.hash illiquidCirculationSupplyValidator'

    value = unwrap >>> _.amount $ snd utxo

  newValue <-
    fromMaybeThrow
      (GenericInternalError "Couldn't add values.")
      $ pure (value `Value.add` depositedValue)

  let
    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator illiquidCirculationSupplyValidator'

    constraints =
      TxConstraints.mustPayToScript
        icsValidatorHash
        PlutusData.unit
        DatumInline
        newValue
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (RedeemerDatum $ toData DepositMoreToSupply)

  void $ balanceSignAndSubmit
    "Illiquid circulation supply DepositMoreToSupply transaction"
    { constraints, lookups }

withdrawFromSupply ::
  forall r.
  SidechainParams ->
  ScriptHash ->
  Value ->
  (TransactionInput /\ TransactionOutput) ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
withdrawFromSupply sp mintingPolicyHash withdrawnValue utxo = do
  versionOracleConfig <- Versioning.getVersionOracleConfig sp

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints sp

  illiquidCirculationSupplyValidator' <- illiquidCirculationSupplyValidator
    versionOracleConfig

  (withdrawalPolicyInput /\ withdrawalPolicyOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: IlliquidCirculationSupplyWithdrawalPolicy
          }
      )

  let
    icsValidatorHash = PlutusScript.hash illiquidCirculationSupplyValidator'

    value = unwrap >>> _.amount $ snd utxo

  newValue <-
    fromMaybeThrow
      (GenericInternalError "Couldn't subtract values.")
      $ pure (value `Value.minus` withdrawnValue)

  let
    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator illiquidCirculationSupplyValidator'
        <> icsLookups

    constraints =
      TxConstraints.mustPayToScript
        icsValidatorHash
        PlutusData.unit
        DatumInline
        newValue
        <> TxConstraints.mustSpendScriptOutput (fst utxo)
          (RedeemerDatum $ toData WithdrawFromSupply)
        <> TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          mintingPolicyHash
          (RedeemerDatum PlutusData.unit)
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
