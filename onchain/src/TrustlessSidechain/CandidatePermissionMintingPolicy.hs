{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
  mkCandidatePermissionMintingPolicyUntyped,
  serialisableCandidatePermissionMintingPolicy,
) where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs),
  TxOutRef,
 )
import PlutusTx qualified
import PlutusTx.Prelude
import TrustlessSidechain.Types (CandidatePermissionMint, cpmUtxo)

{- | 'mkCandidatePermissionMintingPolicy' is a minting policy which verifies:

      - The UTxO 'cpmUtxo' in 'CandidatePermissionMint' is spent
-}
mkCandidatePermissionMintingPolicy ::
  CandidatePermissionMint ->
  () ->
  ScriptContext ->
  Bool
mkCandidatePermissionMintingPolicy cpm _red ctx =
  traceIfFalse "error 'mkCandidatePermissionMintingPolicy' CandidatePermissionMintingPolicy 'cpmUtxo' not consumed" $
    go $ txInfoInputs $ scriptContextTxInfo ctx
  where
    utxo :: TxOutRef
    utxo = cpmUtxo cpm

    -- Tests if any of the input utxos in the script context are equal to the
    -- distinguished UTxO given in @cpm@.
    go :: [TxInInfo] -> Bool
    go = \case
      [] -> False
      (txIn : txIns) -> (utxo == txInInfoOutRef txIn) || go txIns

-- Ctl hack..
mkCandidatePermissionMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCandidatePermissionMintingPolicyUntyped =
  ScriptUtils.mkUntypedMintingPolicy
    . mkCandidatePermissionMintingPolicy
    . PlutusTx.unsafeFromBuiltinData

serialisableCandidatePermissionMintingPolicy :: Versioned Ledger.Script
serialisableCandidatePermissionMintingPolicy =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCandidatePermissionMintingPolicyUntyped||])) PlutusV2
