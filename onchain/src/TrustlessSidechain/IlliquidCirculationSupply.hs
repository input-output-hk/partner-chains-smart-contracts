{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

{- |
Module      : TrustlessSidechain.IlliquidCirculationSupply
Description : Illiquid Circulation Supply validator and auth token minting policy.
-}
module TrustlessSidechain.IlliquidCirculationSupply (
  -- * Illiquid Circulation Supply (ICS) validator
  -- $icsValidator
  mkIlliquidCirculationSupplyValidator,
  mkIlliquidCirculationSupplyValidatorUntyped,
  compiledValidator,
  serialisableIlliquidCirculationSupplyValidator,

  -- * ICS Auth token minting policy
  -- $icsMintingPolicy
  mkIlliquidCirculationSupplyAuthorityTokenPolicy,
  mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped,
  compiledAuthorityTokenPolicy,
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

{- $icsValidator

There can be multiple ICS UTXOs at the validator address; they are identified by having the ICS Auth token.

Redeemers:

1. `DepositMoreToSupply` allows depositing native tokens into a single continuing ICS UTXO.

    * Amount of native tokens in the ICS UTXO must not decrease.
    * Outputs exactly one ICS UTXO, with a single ICS Auth token.
    * Only outputs at the ICS validator address may receive an ICS Auth token.
    * Required reference inputs:

        * `VersionOracle` for `ScriptId.illiquidCirculationSupplyAuthorityTokenPolicyId`

2. `WithdrawFromSupply` allows withdrawing native tokens from the ICS.

    * May spend and output one or more ICS UTXOs.
    * Only outputs at the ICS validator address may receive an ICS Auth token.
    * Conditions of withdrawing tokens are encoded in the ICS supply withdrawal policy, which must exist in the
      versioning system; one withdrawal token must be minted to prove the conditions are fulfilled.
    * Required reference inputs:

        * `VersionOracle` for `ScriptId.illiquidCirculationSupplyWithdrawalPolicyId`

Error codes:

* ERROR-ILLIQUID-CIRCULATION-SUPPLY-01: Output UTxO doesn't have exactly one ICS Authority Token or ICS auth tokens leak from the ICS validator
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-02: Assets of the supply UTxO decreased
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-03: Single illiquid circulation supply token is not minted
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-04: No unique output UTxO at the supply address
* ERROR-ILLIQUID-CIRCULATION-SUPPLY-05: No own input UTxO at the supply address
-}
mkIlliquidCirculationSupplyValidator ::
  VersionOracleConfig ->
  BuiltinData ->
  IlliquidCirculationSupplyRedeemer ->
  ScriptContext ->
  Bool
mkIlliquidCirculationSupplyValidator voc _ red ctx = case red of
  DepositMoreToSupply ->
    icsAuthTokensAreCorrectlyAssigned
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-02" assetsDoNotDecrease
  WithdrawFromSupply ->
    icsAuthTokensAreCorrectlyAssigned
      && traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-03" oneIcsWithdrawalMintingPolicyTokenIsMinted
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

    containsICSAuthorityTokens :: Integer -> TxOut -> Bool
    containsICSAuthorityTokens n TxOut {txOutValue = value} =
      valueOf value icsAuthorityTokenCurrencySymbol icsAuthorityTokenName == n

    supplyInputUtxos :: List.List TxOut
    supplyInputUtxos = Utils.getInputsAt info ownAddress

    supplyInputValue :: Value
    supplyInputValue = List.mconcat (List.map txOutValue supplyInputUtxos)

    relevantTokensOnInput :: Value
    relevantTokensOnInput =
      Value $ Map.delete icsAuthorityTokenCurrencySymbol $ Map.delete adaSymbol $ getValue supplyInputValue

    supplyOutputUtxo :: TxOut
    supplyOutputUtxo =
      Utils.fromSingletonData (\_ -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-04") $ getContinuingOutputs ctx

    relevantTokensOnOutput :: Value
    relevantTokensOnOutput =
      Value $ Map.delete icsAuthorityTokenCurrencySymbol $ Map.delete adaSymbol $ getValue (txOutValue supplyOutputUtxo)

    assetsDoNotDecrease :: Bool
    assetsDoNotDecrease = relevantTokensOnInput `leq` relevantTokensOnOutput

    ownAddress :: Address
    ownAddress = case findOwnInput ctx of
      Just txOut -> txOutAddress $ txInInfoResolved txOut
      Nothing -> traceError "ERROR-ILLIQUID-CIRCULATION-SUPPLY-05"

    -- Outputs are either ICS UTXOs at the ICS validator address, carrying 1 ICS Auth token,
    -- or are UTXOs going to another address, carrying 0 ICS Auth tokens.
    icsAuthTokensAreCorrectlyAssigned :: Bool
    icsAuthTokensAreCorrectlyAssigned =
      traceIfFalse "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01"
        $ List.all
          ( \txOut ->
              ( txOutAddress txOut
                  == ownAddress
                  && containsICSAuthorityTokens 1 txOut
              )
                || ( txOutAddress txOut
                      /= ownAddress
                      && containsICSAuthorityTokens 0 txOut
                   )
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

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||mkIlliquidCirculationSupplyValidatorUntyped||])

serialisableIlliquidCirculationSupplyValidator :: SerialisedScript
serialisableIlliquidCirculationSupplyValidator = serialiseCompiledCode compiledValidator

{- $icsMintingPolicy

Tokens minted with this policy are used for identifying authorized ICS UTXOs at the ICS validator address.

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

compiledAuthorityTokenPolicy :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledAuthorityTokenPolicy = $$(PlutusTx.compile [||mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped||])

serialisableIlliquidCirculationSupplyAuthorityTokenPolicy :: SerialisedScript
serialisableIlliquidCirculationSupplyAuthorityTokenPolicy = serialiseCompiledCode compiledAuthorityTokenPolicy
