{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.IlliquidCirculationSupply (
  mkIlliquidCirculationSupplyValidator,
  mkIlliquidCirculationSupplyValidatorUntyped,
  serialisableIlliquidCirculationSupplyValidator,
) where

import PlutusLedgerApi.Data.V2 (
  Address,
  ScriptContext,
  SerialisedScript,
  TxInfo,
  TxOut,
  getDatum,
  scriptContextTxInfo,
  serialiseCompiledCode,
  txOutAddress,
  txOutDatum,
  txOutValue,
  pattern OutputDatum,
  pattern TxOut,
 )
import PlutusLedgerApi.V1.Data.Value (
  CurrencySymbol,
  TokenName (..),
  lt,
 )
import PlutusLedgerApi.V2.Data.Contexts (getContinuingOutputs)
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types (
  IlliquidCirculationSupplyRedeemer (..),
  VersionedGenericDatum,
 )
import TrustlessSidechain.Utils (oneTokenMinted)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId),
  VersionOracleConfig,
  getVersionedCurrencySymbol,
 )

icsWithdrawalMintingPolicyTokenName :: TokenName
icsWithdrawalMintingPolicyTokenName = TokenName emptyByteString

-- | Error codes description follows:
--
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-01: Input UTxO has non-unit datum
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-02: Output UTxO has non-unit datum
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-03: Assets of the supply UTxO decreased
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-04: Single illiquid circulation supply token is not minted
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-05: No unique input UTxO at the supply address
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-06: No unique output UTxO at the supply address
mkIlliquidCirculationSupplyValidator ::
  VersionOracleConfig ->
  BuiltinData ->
  IlliquidCirculationSupplyRedeemer ->
  ScriptContext ->
  Bool
mkIlliquidCirculationSupplyValidator voc _ red ctx = case red of
  DepositMoreToSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" (isDatumUnit supplyInputUtxo)
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-02" (isDatumUnit supplyOutputUtxo)
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-03" assetsIncrease
  WithdrawFromSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-04" oneIcsWithdrawalMintingPolicyTokenIsMinted
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    icsWithdrawalPolicyCurrencySymbol :: CurrencySymbol
    icsWithdrawalPolicyCurrencySymbol =
      getVersionedCurrencySymbol
        voc
        (VersionOracle {scriptId = ScriptId.illiquidCirculationSupplyWithdrawalPolicyId})
        ctx

    supplyAddress :: Address
    supplyAddress = txOutAddress supplyOutputUtxo

    supplyInputUtxo :: TxOut
    supplyInputUtxo =
      Utils.fromSingletonData "ERROR-ILLIQUID-CIRCULATION-SUPPLY-05"
        $ Utils.getInputsAt info supplyAddress

    supplyOutputUtxo :: TxOut
    supplyOutputUtxo =
      Utils.fromSingletonData "ERROR-ILLIQUID-CIRCULATION-SUPPLY-06"
        $ getContinuingOutputs ctx

    assetsIncrease :: Bool
    assetsIncrease =
      (txOutValue supplyInputUtxo)
        `lt` (txOutValue supplyOutputUtxo)

    oneIcsWithdrawalMintingPolicyTokenIsMinted :: Bool
    oneIcsWithdrawalMintingPolicyTokenIsMinted =
      oneTokenMinted
        info
        icsWithdrawalPolicyCurrencySymbol
        icsWithdrawalMintingPolicyTokenName

    isDatumUnit :: TxOut -> Bool
    isDatumUnit TxOut {txOutDatum = OutputDatum datum} =
      isJust . PlutusTx.fromBuiltinData @(VersionedGenericDatum ()) $ getDatum datum
    isDatumUnit _ = False

mkIlliquidCirculationSupplyValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkIlliquidCirculationSupplyValidatorUntyped voc rd rr ctx =
  check
    $ mkIlliquidCirculationSupplyValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableIlliquidCirculationSupplyValidator :: SerialisedScript
serialisableIlliquidCirculationSupplyValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkIlliquidCirculationSupplyValidatorUntyped||])
