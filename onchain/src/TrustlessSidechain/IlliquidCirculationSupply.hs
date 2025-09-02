{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : TrustlessSidechain.IlliquidCirculationSupply
Description : Illiquid Circulation Supply validator and auth token minting policy.
-}
module TrustlessSidechain.IlliquidCirculationSupply (
  mkIlliquidCirculationSupplyValidator,
  mkIlliquidCirculationSupplyValidatorUntyped,
  serialisableIlliquidCirculationSupplyValidator,
  mkIlliquidCirculationSupplyAuthorityTokenPolicy,
  mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped,
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

{- |
Illiquid Circulation Supply (ICS) validator

There can be multiple ICS UTXOs at the validator address; they are identified by having the ICS Auth token.

Redeemers:

1. `DepositMoreToSupply` allows depositing PC tokens into the ICS.

    * Amount of PC tokens in the ICS UTXO must not decrease.
    * Output ICS UTXO must contain single ICS Auth token.
    * Only outputs at the ICS validator address may receive an ICS Auth token.
    * Required reference inputs:

        * `VersionOracle` for `ScriptId.illiquidCirculationSupplyAuthorityTokenPolicyId`

2. `WithdrawFromSupply` allows withdrawing PC tokens from the ICS.

    * Conditions of withdrawing tokens are encoded in the ICS supply withdrawal policy, which must exist in the
      versioning system; one withdrawal token must be minted to prove the conditions are fulfilled.
    * Required reference inputs:

        * `VersionOracle` for `ScriptId.illiquidCirculationSupplyWithdrawalPolicyId`

Error codes:

* ERROR-ILLIQUID-CIRCULATION-SUPPLY-01: Output UTxO doesn't have exactly one ICS Authority Token
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-02: Assets of the supply UTxO decreased
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-03: ICS auth tokens leak from the ICS validator
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-04: Single illiquid circulation supply token is not minted
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-05: No unique output UTxO at the supply address
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-06: No own input UTxO at the supply address
-}
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

    relevantTokensOnInput :: Value
    relevantTokensOnInput =
      Value $ Map.delete icsAuthorityTokenCurrencySymbol $ Map.delete adaSymbol $ getValue supplyInputValue

    supplyOutputUtxo :: TxOut
    supplyOutputUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-05")
        $ getContinuingOutputs ctx

    relevantTokensOnOutput :: Value
    relevantTokensOnOutput =
      Value $ Map.delete icsAuthorityTokenCurrencySymbol $ Map.delete adaSymbol $ getValue (txOutValue supplyOutputUtxo)

    assetsDoNotDecrease :: Bool
    assetsDoNotDecrease = relevantTokensOnInput `leq` relevantTokensOnOutput

    ownAddress :: Address
    ownAddress = case findOwnInput ctx of
      Just txOut -> txOutAddress $ txInInfoResolved txOut
      Nothing -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-06"

    icsAuthTokensDoNotLeakFromIcs :: Bool
    icsAuthTokensDoNotLeakFromIcs =
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

{- |
ICS Auth token minting policy

Tokens minted with this policy are used to identify ICS UTXOs at the ICS validator address.

Error codes:

* ERROR-ICS-AUTH-TOKEN-01: No authority signature.
-}
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
