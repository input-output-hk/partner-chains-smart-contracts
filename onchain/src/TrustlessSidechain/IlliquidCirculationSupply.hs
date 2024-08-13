{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.IlliquidCirculationSupply (
  mkIlliquidCirculationSupplyValidator,
  mkIlliquidCirculationSupplyValidatorUntyped,
  serialisableIlliquidCirculationSupplyValidator,
) where

import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName (..),
  Value,
  lt,
 )
import Plutus.V2.Ledger.Api (
  Address,
  Script,
  fromCompiledCode,
  getDatum,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  IlliquidCirculationSupplyRedeemer (..),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (oneTokenMinted)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  getVersionedCurrencySymbolUnsafe,
  illiquidCirculationSupplyWithdrawalPolicyId,
 )

icsWithdrawalMintingPolicyTokenName :: TokenName
icsWithdrawalMintingPolicyTokenName = TokenName emptyByteString

{-# INLINEABLE getInputsAt #-}
getInputsAt :: Unsafe.TxInfo -> Address -> [Unsafe.TxOut]
getInputsAt txInfo address =
  Unsafe.txInInfoResolved
    <$> ((== address) . Unsafe.decode . Unsafe.txOutAddress . Unsafe.txInInfoResolved)
    `filter` Unsafe.txInfoInputs txInfo

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
  Unsafe.ScriptContext ->
  Bool
mkIlliquidCirculationSupplyValidator voc _ red ctx = case red of
  DepositMoreToSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" (isDatumUnit supplyInputUtxo)
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-02" (isDatumUnit supplyOutputUtxo)
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-03" assetsIncrease
  WithdrawFromSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-04" oneIcsWithdrawalMintingPolicyTokenIsMinted
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx

    icsWithdrawalPolicyCurrencySymbol :: CurrencySymbol
    icsWithdrawalPolicyCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        voc
        (VersionOracle {version = 1, scriptId = illiquidCirculationSupplyWithdrawalPolicyId})
        ctx

    minted :: Value
    minted = Unsafe.decode . Unsafe.txInfoMint $ info

    supplyAddress :: Address
    supplyAddress = Unsafe.decode $ Unsafe.txOutAddress supplyOutputUtxo

    supplyInputUtxo :: Unsafe.TxOut
    supplyInputUtxo =
      Utils.fromSingleton "ERROR-ILLIQUID-CIRCULATION-SUPPLY-05" $
        getInputsAt info supplyAddress

    supplyOutputUtxo :: Unsafe.TxOut
    supplyOutputUtxo =
      Utils.fromSingleton "ERROR-ILLIQUID-CIRCULATION-SUPPLY-06" $
        Unsafe.getContinuingOutputs ctx

    assetsIncrease :: Bool
    assetsIncrease =
      (Unsafe.decode . Unsafe.txOutValue $ supplyInputUtxo)
      `lt`
      (Unsafe.decode . Unsafe.txOutValue $ supplyOutputUtxo)

    oneIcsWithdrawalMintingPolicyTokenIsMinted :: Bool
    oneIcsWithdrawalMintingPolicyTokenIsMinted =
      oneTokenMinted
        minted
        icsWithdrawalPolicyCurrencySymbol
        icsWithdrawalMintingPolicyTokenName

    isDatumUnit :: Unsafe.TxOut -> Bool
    isDatumUnit txOut =
      isJust
        (getDatum . Unsafe.decode
          <$> (Unsafe.getOutputDatum . Unsafe.txOutDatum) txOut
          >>= PlutusTx.fromBuiltinData @())

mkIlliquidCirculationSupplyValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkIlliquidCirculationSupplyValidatorUntyped voc rd rr ctx =
  check $
    mkIlliquidCirculationSupplyValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (Unsafe.wrap ctx)

serialisableIlliquidCirculationSupplyValidator :: Script
serialisableIlliquidCirculationSupplyValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkIlliquidCirculationSupplyValidatorUntyped||])
