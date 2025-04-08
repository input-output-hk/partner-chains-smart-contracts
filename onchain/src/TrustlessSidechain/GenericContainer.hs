{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.GenericContainer (
  serialisableMintingPolicy,
  serialisableValidator,
  genericContainerValidator,
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
--   ERROR-GENERIC-CONTAINER-POLICY-01: transaction not signed by the governance
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
--   ERROR-GENERIC-CONTAINER-VALIDATOR-01: transaction not signed by the governance
--   authority
--
{-# INLINEABLE genericContainerValidator #-}
genericContainerValidator ::
  BuiltinData ->
  VersionOracleConfig ->
  BuiltinData ->
  BuiltinByteString ->
  Unsafe.ScriptContext ->
  Bool
genericContainerValidator
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
    $ genericContainerValidator
      genesisUtxo
      (unsafeFromBuiltinData vc)
      dat
      (unsafeFromBuiltinData redeemer)
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
