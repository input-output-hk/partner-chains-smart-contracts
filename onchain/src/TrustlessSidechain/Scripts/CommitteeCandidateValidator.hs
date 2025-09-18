{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Scripts.CommitteeCandidateValidator (
  mkCommitteeCandidateValidator,
  committeeCandidateValidatorUntyped,
  compiledValidator,
  serialisableValidator,
) where

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Data.V2 (PubKeyHash, ScriptContext, TxInfo, scriptContextTxInfo, serialiseCompiledCode)
import PlutusLedgerApi.V2.Data.Contexts (txSignedBy)
import PlutusTx qualified
import PlutusTx.Prelude
import TrustlessSidechain.Types (VersionedGenericDatum (..))

{-# INLINEABLE mkCommitteeCandidateValidator #-}
-- OnChain error descriptions:
--
--   ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01: Transaction not signed by the
--   original submitter.
mkCommitteeCandidateValidator ::
  BuiltinData ->
  VersionedGenericDatum PubKeyHash ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkCommitteeCandidateValidator _genesisUtxo (VersionedGenericDatum {datum = pkh}) _redeemer ctx =
  traceIfFalse "ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01" isSigned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    isSigned :: Bool
    isSigned = txSignedBy info pkh

{-# INLINEABLE committeeCandidateValidatorUntyped #-}
committeeCandidateValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinUnit
committeeCandidateValidatorUntyped genesisUtxo datum red ctx =
  check
    $ mkCommitteeCandidateValidator
      genesisUtxo
      (unsafeFromBuiltinData datum)
      red
      (unsafeFromBuiltinData ctx)

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])

serialisableValidator :: SerialisedScript
serialisableValidator = serialiseCompiledCode compiledValidator
