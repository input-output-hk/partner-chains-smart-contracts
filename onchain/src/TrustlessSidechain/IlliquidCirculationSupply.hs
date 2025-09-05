{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.IlliquidCirculationSupply (
  mkIlliquidCirculationSupplyValidatorUntyped,
  mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped,
  compiledValidator,
  compiledAuthorityTokenPolicy,
  serialisableIlliquidCirculationSupplyValidator,
  serialisableIlliquidCirculationSupplyAuthorityTokenPolicy,
) where

import PlutusLedgerApi.Data.V2 (
  Address,
  ScriptContext,
  SerialisedScript,
  TxInfo,
  TxOut,
  adaSymbol,
  scriptContextTxInfo,
  serialiseCompiledCode,
  txInfoOutputs,
  txOutAddress,
  txOutValue,
  pattern TxOut,
 )
import PlutusLedgerApi.V1.Data.Value (
  CurrencySymbol,
  TokenName (..),
  Value (..),
  leq,
  valueOf,
 )
import PlutusLedgerApi.V2.Data.Contexts (findOwnInput, getContinuingOutputs, txInInfoResolved)
import PlutusTx qualified
import PlutusTx.Data.AssocMap qualified as Map
import PlutusTx.Data.List qualified as List
import PlutusTx.Prelude
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types (
  IlliquidCirculationSupplyRedeemer (..),
  VersionOracle (VersionOracle, scriptId),
  VersionOracleConfig,
 )
import TrustlessSidechain.Utils (oneTokenMinted)
import TrustlessSidechain.Utils qualified as Utils
import TrustlessSidechain.Versioning (
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
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-02: Assets of the supply UTxO decreased
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-03: ICS auth tokens leak from the ICS validator
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-04: Single illiquid circulation supply token is not minted
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-05: No unique output UTxO at the supply address
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-06: Some output UTxO at the validator address that doesn't have exactly one ICS Authority Token
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-07: ICS auth tokens leak from the ICS validator
--   ERROR-ILLIQUID-CIRCULATION-SUPPLY-08: No own input UTxO at the supply address
mkIlliquidCirculationSupplyValidator ::
  VersionOracleConfig ->
  BuiltinData ->
  IlliquidCirculationSupplyRedeemer ->
  ScriptContext ->
  Bool
mkIlliquidCirculationSupplyValidator voc _ red ctx = case red of
  DepositMoreToSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" (containsOnlyOneICSAuthorityToken supplyOutputUtxo)
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-02" assetsDoNotDecrease
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-03" icsAuthTokensDoNotLeakFromIcs
  WithdrawFromSupply ->
    traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-04" oneIcsWithdrawalMintingPolicyTokenIsMinted
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-06" eachIcsOutputHasExactlyOneIcsAuthToken
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-07" icsAuthTokensDoNotLeakFromIcs
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

    supplyInputUtxos :: List.List TxOut
    supplyInputUtxos = Utils.getInputsAt info ownAddress

    supplyInputValue :: Value
    supplyInputValue = List.mconcat (List.map txOutValue supplyInputUtxos)

    relevantTokensOnInput :: Value
    relevantTokensOnInput =
      Value $ Map.delete icsAuthorityTokenCurrencySymbol $ Map.delete adaSymbol $ getValue supplyInputValue

    supplyOutputUtxo :: TxOut
    supplyOutputUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-05") continuingOutputs

    relevantTokensOnOutput :: Value
    relevantTokensOnOutput =
      Value $ Map.delete icsAuthorityTokenCurrencySymbol $ Map.delete adaSymbol $ getValue (txOutValue supplyOutputUtxo)

    assetsDoNotDecrease :: Bool
    assetsDoNotDecrease = relevantTokensOnInput `leq` relevantTokensOnOutput

    ownAddress :: Address
    ownAddress = case findOwnInput ctx of
      Just txOut -> txOutAddress $ txInInfoResolved txOut
      Nothing -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-08"

    icsAuthTokensDoNotLeakFromIcs :: Bool
    icsAuthTokensDoNotLeakFromIcs =
      List.all
        ( \txOut ->
            txOutAddress txOut
              == ownAddress
              || valueOf (txOutValue txOut) icsAuthorityTokenCurrencySymbol icsAuthorityTokenName
              == 0
        )
        $ txInfoOutputs info

    oneIcsWithdrawalMintingPolicyTokenIsMinted :: Bool
    oneIcsWithdrawalMintingPolicyTokenIsMinted =
      oneTokenMinted
        info
        icsWithdrawalPolicyCurrencySymbol
        icsWithdrawalMintingPolicyTokenName

    eachIcsOutputHasExactlyOneIcsAuthToken :: Bool
    eachIcsOutputHasExactlyOneIcsAuthToken =
      List.all containsOnlyOneICSAuthorityToken continuingOutputs

    continuingOutputs :: List.List TxOut
    !continuingOutputs = getContinuingOutputs ctx

mkIlliquidCirculationSupplyValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkIlliquidCirculationSupplyValidatorUntyped voc rd rr ctx =
  check
    $ mkIlliquidCirculationSupplyValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (PlutusTx.unsafeFromBuiltinData ctx)

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||mkIlliquidCirculationSupplyValidatorUntyped||])

serialisableIlliquidCirculationSupplyValidator :: SerialisedScript
serialisableIlliquidCirculationSupplyValidator = serialiseCompiledCode compiledValidator

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

compiledAuthorityTokenPolicy :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledAuthorityTokenPolicy = $$(PlutusTx.compile [||mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped||])

serialisableIlliquidCirculationSupplyAuthorityTokenPolicy :: SerialisedScript
serialisableIlliquidCirculationSupplyAuthorityTokenPolicy = serialiseCompiledCode compiledAuthorityTokenPolicy
