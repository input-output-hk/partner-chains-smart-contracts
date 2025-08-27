{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.IlliquidCirculationSupply (
  mkIlliquidCirculationSupplyValidatorUntyped,
  mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped,
  serialisableIlliquidCirculationSupplyValidator,
  serialisableIlliquidCirculationSupplyAuthorityTokenPolicy,
) where

import PlutusLedgerApi.Data.V2 (
  Address,
  ScriptContext,
  SerialisedScript,
  TxInfo,
  TxOut,
  scriptContextTxInfo,
  serialiseCompiledCode,
  txInfoOutputs,
  txOutAddress,
  txOutValue,
  pattern TxOut,
 )
import PlutusLedgerApi.V1.Data.Value (
  AssetClass,
  CurrencySymbol,
  TokenName (..),
  Value,
  assetClassValueOf,
  valueOf,
 )
import PlutusLedgerApi.V2.Data.Contexts (findOwnInput, getContinuingOutputs, txInInfoResolved)
import PlutusTx qualified
import PlutusTx.Data.List qualified as List
import PlutusTx.Prelude
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types (
  IlliquidCirculationSupplyRedeemer (..),
 )
import TrustlessSidechain.Utils (oneTokenMinted)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId),
  VersionOracleConfig,
  approvedByGovernance,
  getVersionedCurrencySymbol,
 )

icsWithdrawalMintingPolicyTokenName :: TokenName
icsWithdrawalMintingPolicyTokenName = TokenName emptyByteString

icsAuthorityTokenName :: TokenName
icsAuthorityTokenName = TokenName emptyByteString

-- | Error codes description follows:
--
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-01: Output UTxO doesn't have exactly one ICS Authority Token
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-03: Assets of the supply UTxO decreased
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-04: Reserve tokens leak from the ICS validator
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-05: Single illiquid circulation supply token is not minted
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-06: No unique output UTxO at the supply address
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-07: No own input UTxO at the supply address
mkIlliquidCirculationSupplyValidator ::
  VersionOracleConfig ->
  AssetClass ->
  BuiltinData ->
  IlliquidCirculationSupplyRedeemer ->
  ScriptContext ->
  Bool
mkIlliquidCirculationSupplyValidator voc reserveToken _ red ctx = case red of
  DepositMoreToSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" (containsOnlyOneICSAuthorityToken supplyOutputUtxo)
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-03" assetsDoNotDecrease
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-08" reserveTokensDoNotLeakFromIcs
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

    icsAuthorityTokenCurrencySymbol :: CurrencySymbol
    icsAuthorityTokenCurrencySymbol =
      getVersionedCurrencySymbol
        voc
        (VersionOracle {scriptId = ScriptId.illiquidCirculationSupplyAuthorityTokenPolicyId})
        ctx

    containsOnlyOneICSAuthorityToken :: TxOut -> Bool
    containsOnlyOneICSAuthorityToken TxOut {txOutValue = value} =
      let valueOfIcsAuthorityToken = valueOf value icsAuthorityTokenCurrencySymbol icsAuthorityTokenName
       in valueOfIcsAuthorityToken == 1

    supplyAddress :: Address
    supplyAddress = txOutAddress supplyOutputUtxo

    supplyInputUtxos :: List.List TxOut
    supplyInputUtxos = Utils.getInputsAt info supplyAddress

    supplyInputValue :: Value
    supplyInputValue = List.mconcat (List.map txOutValue supplyInputUtxos)

    inputReserveTokens :: Integer
    inputReserveTokens = assetClassValueOf supplyInputValue reserveToken

    supplyOutputUtxo :: TxOut
    supplyOutputUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-06")
        $ getContinuingOutputs ctx

    supplyOutputReserveTokens :: Integer
    supplyOutputReserveTokens = assetClassValueOf (txOutValue supplyOutputUtxo) reserveToken

    assetsDoNotDecrease :: Bool
    assetsDoNotDecrease = inputReserveTokens <= supplyOutputReserveTokens

    ownAddress :: Address
    ownAddress = txOutAddress $ txInInfoResolved $ case findOwnInput ctx of
      Just txOut -> txOut
      Nothing -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-07"

    reserveTokensDoNotLeakFromIcs :: Bool
    reserveTokensDoNotLeakFromIcs =
      List.null
        $ List.filter
          ( \txOut ->
              txOutAddress txOut
                /= ownAddress
                && valueOf (txOutValue txOut) icsAuthorityTokenCurrencySymbol icsAuthorityTokenName
                > 0
          )
        $ txInfoOutputs info

    oneIcsWithdrawalMintingPolicyTokenIsMinted :: Bool
    oneIcsWithdrawalMintingPolicyTokenIsMinted =
      oneTokenMinted
        info
        icsWithdrawalPolicyCurrencySymbol
        icsWithdrawalMintingPolicyTokenName

mkIlliquidCirculationSupplyValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkIlliquidCirculationSupplyValidatorUntyped voc rt rd rr ctx =
  check
    $ mkIlliquidCirculationSupplyValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rt)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableIlliquidCirculationSupplyValidator :: SerialisedScript
serialisableIlliquidCirculationSupplyValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkIlliquidCirculationSupplyValidatorUntyped||])

mkIlliquidCirculationSupplyAuthorityTokenPolicy :: BuiltinData -> VersionOracleConfig -> BuiltinData -> ScriptContext -> Bool
mkIlliquidCirculationSupplyAuthorityTokenPolicy _scriptId voc _ ctx =
  traceIfFalse "ERROR-ICS-AUTH-TOKEN-01" signedByAuthority
  where
    signedByAuthority :: Bool
    signedByAuthority =
      approvedByGovernance voc ctx

mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped _scriptId voc rd ctx =
  check
    $ mkIlliquidCirculationSupplyAuthorityTokenPolicy
      _scriptId
      (PlutusTx.unsafeFromBuiltinData voc)
      rd
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableIlliquidCirculationSupplyAuthorityTokenPolicy :: SerialisedScript
serialisableIlliquidCirculationSupplyAuthorityTokenPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped||])
