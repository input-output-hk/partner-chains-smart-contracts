{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.GovernedMap (
  serialisableMintingPolicy,
  serialisableValidator,
  governedMapValidator,
  mkMintingPolicy,
  mkMintingPolicyUntyped,
  mkValidatorUntyped,
) where

import PlutusLedgerApi.Data.V2 (
  ScriptContext,
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Versioning (VersionOracleConfig, approvedByGovernance)

-- OnChain error descriptions:
--
--   ERROR-GOVERNED-MAP-POLICY-01: transaction not signed by the governance
--   authority
--
mkMintingPolicy ::
  BuiltinData ->
  BuiltinData ->
  VersionOracleConfig ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkMintingPolicy
  _scriptId
  _genesisUtxo
  vc
  _redeemer
  ctx =
    let
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx
     in
      traceIfFalse "ERROR-GOVERNED-MAP-POLICY-01" signedByGovernanceAuthority

-- OnChain error descriptions:
--
--   ERROR-GOVERNED-MAP-VALIDATOR-01: transaction not signed by the governance
--   authority
--
{-# INLINEABLE governedMapValidator #-}
governedMapValidator ::
  BuiltinData ->
  BuiltinData ->
  VersionOracleConfig ->
  BuiltinData ->
  BuiltinData ->
  ScriptContext ->
  Bool
governedMapValidator
  _scriptId
  _genesisUtxo
  vc
  _
  _redeemer
  ctx =
    traceIfFalse "ERROR-GOVERNED-MAP-VALIDATOR-01" signedByGovernanceAuthority
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
  BuiltinData ->
  BuiltinUnit
mkValidatorUntyped scriptId genesisUtxo vc dat redeemer ctx =
  check
    $ governedMapValidator
      scriptId
      genesisUtxo
      (unsafeFromBuiltinData vc)
      dat
      redeemer
      (unsafeFromBuiltinData ctx)

serialisableValidator :: SerialisedScript
serialisableValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])

mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinUnit
mkMintingPolicyUntyped scriptId genesisUtxo vc redeemer ctx =
  check
    $ mkMintingPolicy
      scriptId
      genesisUtxo
      (unsafeFromBuiltinData vc)
      redeemer
      (unsafeFromBuiltinData ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
