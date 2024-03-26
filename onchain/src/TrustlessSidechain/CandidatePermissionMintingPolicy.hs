{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
  mkCandidatePermissionMintingPolicyUntyped,
  serialisableCandidatePermissionMintingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (InitTokenAssetClass)
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (
  oneTokenBurned,
 )

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
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkCandidatePermissionMintingPolicy itac _ scriptContext =
  traceIfFalse "ERROR-CANDIDATE-PERMISSION-POLICY-01" initTokenBurned
  where
    mint =
      Unsafe.decode
        . Unsafe.txInfoMint
        . Unsafe.scriptContextTxInfo
        $ scriptContext

    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        mint
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

mkCandidatePermissionMintingPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkCandidatePermissionMintingPolicyUntyped initTokenAssetClass a scriptContext =
  check $
    mkCandidatePermissionMintingPolicy
      (PlutusTx.unsafeFromBuiltinData initTokenAssetClass)
      a
      (Unsafe.wrap scriptContext)

serialisableCandidatePermissionMintingPolicy :: Script
serialisableCandidatePermissionMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkCandidatePermissionMintingPolicyUntyped||])
