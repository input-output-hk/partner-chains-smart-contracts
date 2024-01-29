{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
  mkCandidatePermissionMintingPolicyUntyped,
  serialisableCandidatePermissionMintingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs),
  TxOutRef,
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (CandidatePermissionMint)
import TrustlessSidechain.Utils (mkUntypedMintingPolicy)

-- | 'mkCandidatePermissionMintingPolicy' is a minting policy which verifies:
--
--      - The UTxO 'cpmUtxo' in 'CandidatePermissionMint' is spent
--
--  OnChain error descriptions:
--
--    ERROR-CANDIDATE-PERMISSION-POLICY-01: UTxO denoted in
--    CandidatePermissionMint was not consumed by the transaction
mkCandidatePermissionMintingPolicy ::
  CandidatePermissionMint ->
  () ->
  ScriptContext ->
  Bool
mkCandidatePermissionMintingPolicy cpm _red ctx =
  traceIfFalse "ERROR-CANDIDATE-PERMISSION-POLICY-01" $
    go $
      txInfoInputs $
        scriptContextTxInfo ctx
  where
    utxo :: TxOutRef
    utxo = get @"utxo" cpm

    -- Tests if any of the input utxos in the script context are equal to the
    -- distinguished UTxO given in @cpm@.
    go :: [TxInInfo] -> Bool
    go = \case
      [] -> False
      (txIn : txIns) -> (utxo == txInInfoOutRef txIn) || go txIns

mkCandidatePermissionMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCandidatePermissionMintingPolicyUntyped =
  mkUntypedMintingPolicy
    . mkCandidatePermissionMintingPolicy
    . PlutusTx.unsafeFromBuiltinData

serialisableCandidatePermissionMintingPolicy :: Script
serialisableCandidatePermissionMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkCandidatePermissionMintingPolicyUntyped||])
