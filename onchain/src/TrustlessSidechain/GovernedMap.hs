{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.GovernedMap (
  serialisableMintingPolicy,
  serialisableValidator,
  governedMapValidator,
  mkMintingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Versioning (VersionOracleConfig, approvedByGovernance)

-- OnChain error descriptions:
--
--   ERROR-GOVERNED-MAP-POLICY-01: transaction not signed by the governance
--   authority
--
mkMintingPolicy ::
  BuiltinData ->
  VersionOracleConfig ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkMintingPolicy
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
      traceIfFalse "ERROR-GENERIC-CONTAINER-POLICY-01" signedByGovernanceAuthority

-- OnChain error descriptions:
--
--   ERROR-GOVERNED-MAP-VALIDATOR-01: transaction not signed by the governance
--   authority
--
{-# INLINEABLE governedMapValidator #-}
governedMapValidator ::
  BuiltinData ->
  VersionOracleConfig ->
  BuiltinData ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
governedMapValidator
  _genesisUtxo
  vc
  _
  _redeemer
  ctx =
    traceIfFalse "ERROR-GENERIC-CONTAINER-VALIDATOR-01" signedByGovernanceAuthority
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
  ()
mkValidatorUntyped genesisUtxo vc dat redeemer ctx =
  check
    $ governedMapValidator
      genesisUtxo
      (unsafeFromBuiltinData vc)
      dat
      redeemer
      (Unsafe.wrap ctx)

serialisableValidator :: SerialisedScript
serialisableValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])

mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkMintingPolicyUntyped genesisUtxo vc redeemer ctx =
  check
    $ mkMintingPolicy
      genesisUtxo
      (unsafeFromBuiltinData vc)
      redeemer
      (Unsafe.wrap ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])
