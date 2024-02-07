{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
  mkCandidatePermissionMintingPolicyUntyped,
  serialisableCandidatePermissionMintingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (InitTokenAssetClass)
import TrustlessSidechain.Utils (mkUntypedMintingPolicy, oneTokenBurned)

-- | 'mkCandidatePermissionMintingPolicy' is a minting policy which verifies:
--
--      - The UTxO 'cpmUtxo' in 'CandidatePermissionMint' is spent
--
--  OnChain error descriptions:
--
--    ERROR-CANDIDATE-PERMISSION-POLICY-01: UTxO denoted in
--    CandidatePermissionMint was not consumed by the transaction
mkCandidatePermissionMintingPolicy ::
  InitTokenAssetClass ->
  () ->
  ScriptContext ->
  Bool
mkCandidatePermissionMintingPolicy itac () ctx =
  traceIfFalse "ERROR-CANDIDATE-PERMISSION-POLICY-01" initTokenBurned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        info
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

mkCandidatePermissionMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCandidatePermissionMintingPolicyUntyped =
  mkUntypedMintingPolicy
    . mkCandidatePermissionMintingPolicy
    . PlutusTx.unsafeFromBuiltinData

serialisableCandidatePermissionMintingPolicy :: Script
serialisableCandidatePermissionMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkCandidatePermissionMintingPolicyUntyped||])
