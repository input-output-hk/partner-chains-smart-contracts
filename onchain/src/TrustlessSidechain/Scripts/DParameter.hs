{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Scripts.DParameter (
  compiledMintingPolicy,
  compiledValidator,
  serialisableMintingPolicy,
  serialisableValidator,
  dParameterValidator,
  mkValidatorUntyped,
  mkMintingPolicy,
  mkMintingPolicyUntyped,
) where

import PlutusLedgerApi.Data.V2 (
  Address,
  ScriptContext,
  SerialisedScript,
  serialiseCompiledCode,
  txInfoMint,
  txInfoOutputs,
  txOutAddress,
  txOutValue,
  pattern Minting,
  pattern ScriptContext,
 )
import PlutusLedgerApi.V1.Data.Value (currencySymbolValueOf)
import PlutusTx qualified
import PlutusTx.Data.List qualified as List
import PlutusTx.Foldable (sum)
import PlutusTx.Prelude
import TrustlessSidechain.Scripts.Versioning (approvedByGovernance)
import TrustlessSidechain.Types (VersionOracleConfig)

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-POLICY-01: transaction not signed by the governance
--   authority
--
--   ERROR-DPARAMETER-POLICY-02: some tokens were not sent to the
--   dParameterValidatorAddress
--
--   ERROR-DPARAMETER-POLICY-03: Wrong ScriptContext - this should never happen
mkMintingPolicy ::
  BuiltinData ->
  VersionOracleConfig ->
  Address ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkMintingPolicy
  _genesisUtxo
  vc
  dParameterValidatorAddress
  _redeemer
  ctx@(ScriptContext txInfo (Minting currSym)) =
    let
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx

      -- Amount of DParameterToken sent to the DParameterValidator address
      outAmount :: Integer
      outAmount =
        sum
          [ currencySymbolValueOf value currSym
          | txOut <- List.toSOP $ txInfoOutputs txInfo
          , let address = txOutAddress txOut
          , let value = txOutValue txOut
          , -- look at UTxOs that are sent to the dParameterValidatorAddress
          address == dParameterValidatorAddress
          ]

      -- Amount of DParameterToken minted by this transaction
      mintAmount :: Integer
      mintAmount = currencySymbolValueOf (txInfoMint txInfo) currSym

      -- Check whether the amount of tokens minted is equal to the
      -- amount of tokens sent to the DParameterValidator address
      allTokensSentToDParameterValidator :: Bool
      allTokensSentToDParameterValidator = mintAmount == outAmount
     in
      traceIfFalse "ERROR-DPARAMETER-POLICY-01" signedByGovernanceAuthority
        && traceIfFalse
          "ERROR-DPARAMETER-POLICY-02"
          allTokensSentToDParameterValidator
mkMintingPolicy _ _ _ _ _ = traceError "ERROR-DPARAMETER-POLICY-03"

-- OnChain error descriptions:
--
--   ERROR-DPARAMETER-VALIDATOR-01: transaction not signed by the governance
--   authority
--

{-# INLINEABLE dParameterValidator #-}
dParameterValidator ::
  BuiltinData ->
  -- Here raw BuiltinData is passed to allow to spend from this validator
  -- even if UTxO contains invalid datum
  VersionOracleConfig ->
  BuiltinData -> -- VersionedGenericDatum ()
  BuiltinData ->
  ScriptContext ->
  Bool
dParameterValidator _genesisUtxo vc _dat _redeemer ctx =
  traceIfFalse "ERROR-DPARAMETER-VALIDATOR-01" signedByGovernanceAuthority
  where
    -- Check that transaction was approved by governance authority
    signedByGovernanceAuthority :: Bool
    signedByGovernanceAuthority =
      approvedByGovernance vc ctx

mkValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinUnit
mkValidatorUntyped genesisUtxo vc dat redeemer ctx =
  check
    $ dParameterValidator
      genesisUtxo
      (unsafeFromBuiltinData vc)
      dat
      redeemer
      (unsafeFromBuiltinData ctx)

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||mkValidatorUntyped||])

serialisableValidator :: SerialisedScript
serialisableValidator = serialiseCompiledCode compiledValidator

mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinUnit
mkMintingPolicyUntyped genesisUtxo vc validatorAddress redeemer ctx =
  check
    $ mkMintingPolicy
      genesisUtxo
      (unsafeFromBuiltinData vc)
      (unsafeFromBuiltinData validatorAddress)
      redeemer
      (unsafeFromBuiltinData ctx)

compiledMintingPolicy :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledMintingPolicy = $$(PlutusTx.compile [||mkMintingPolicyUntyped||])

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy = serialiseCompiledCode compiledMintingPolicy
