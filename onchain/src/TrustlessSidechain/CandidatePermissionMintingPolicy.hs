{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
  mkCandidatePermissionMintingPolicyUntyped,
  serialisableCandidatePermissionMintingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  Value,
  fromCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as PtxBI
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (InitTokenAssetClass)
import TrustlessSidechain.Utils (oneTokenBurned)

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
  Value ->
  Bool
mkCandidatePermissionMintingPolicy itac () mint =
  traceIfFalse "ERROR-CANDIDATE-PERMISSION-POLICY-01" initTokenBurned
  where
    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        mint
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

getTxInfoMintFromScriptContext :: BuiltinData -> Value
getTxInfoMintFromScriptContext bd = PlutusTx.unsafeFromBuiltinData mint
  where
    tinfo = head . snd . PtxBI.unsafeDataAsConstr $ bd -- 1st field of ScriptContext is TxInfo
    mint = snd (PtxBI.unsafeDataAsConstr tinfo) !! 4 -- 5th field of TxInfo is txInfoMint

mkCandidatePermissionMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCandidatePermissionMintingPolicyUntyped initTokenAssetClass _ scriptContext = check $ mkCandidatePermissionMintingPolicy (PlutusTx.unsafeFromBuiltinData initTokenAssetClass) () (getTxInfoMintFromScriptContext scriptContext)

serialisableCandidatePermissionMintingPolicy :: Script
serialisableCandidatePermissionMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkCandidatePermissionMintingPolicyUntyped||])
