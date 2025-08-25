{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | This module provides the ICS Authority Token minting policy that allows
-- minting and burning of tokens only when signed by the governance authority.
module TrustlessSidechain.ICSAuthorityToken (
  serialisableMintingPolicy,
  mkMintingPolicy,
  mkMintingPolicyUntyped,
) where

import PlutusLedgerApi.Data.V2 (
  ScriptContext,
  SerialisedScript,
  serialiseCompiledCode,
  pattern Minting,
  pattern ScriptContext,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ICSAuthorityTokenRedeemer (
    ICSAuthorityTokenBurn,
    ICSAuthorityTokenMint
  ),
 )
import TrustlessSidechain.Versioning (VersionOracleConfig, approvedByGovernance)

-- OnChain error descriptions:
--
--   ERROR-ICS-AUTHORITY-TOKEN-POLICY-01: transaction not signed by the
--   governance authority (mint operation)
--
--   ERROR-ICS-AUTHORITY-TOKEN-POLICY-02: transaction not signed by the  
--   governance authority (burn operation)
--
--   ERROR-ICS-AUTHORITY-TOKEN-POLICY-03: Wrong ScriptContext - this should
--   never happen

-- | ICS Authority Token minting policy that enforces governance authority
-- signature for both minting and burning operations.
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy ::
  BuiltinData ->
  VersionOracleConfig ->
  ICSAuthorityTokenRedeemer ->
  ScriptContext ->
  Bool
mkMintingPolicy
  _genesisUtxo
  vc
  ICSAuthorityTokenMint
  ctx@(ScriptContext _txInfo (Minting _currSym)) =
    let
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx
     in
      traceIfFalse "ERROR-ICS-AUTHORITY-TOKEN-POLICY-01" signedByGovernanceAuthority
mkMintingPolicy
  _genesisUtxo
  vc
  ICSAuthorityTokenBurn
  ctx@(ScriptContext _txInfo (Minting _currSym)) =
    let
      -- Check that transaction was approved by governance authority
      signedByGovernanceAuthority :: Bool
      signedByGovernanceAuthority =
        approvedByGovernance vc ctx
     in
      traceIfFalse "ERROR-ICS-AUTHORITY-TOKEN-POLICY-02" signedByGovernanceAuthority
mkMintingPolicy _ _ _ _ = traceError "ERROR-ICS-AUTHORITY-TOKEN-POLICY-03"

{-# INLINEABLE mkMintingPolicyUntyped #-}
mkMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinUnit
mkMintingPolicyUntyped genesisUtxo vc redeemer ctx =
  check
    $ mkMintingPolicy
      genesisUtxo
      (unsafeFromBuiltinData vc)
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

serialisableMintingPolicy :: SerialisedScript
serialisableMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkMintingPolicyUntyped||])