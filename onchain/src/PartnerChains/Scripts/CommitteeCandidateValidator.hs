{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : PartnerChains.Scripts.CommitteeCandidateValidator
Description : Committee candidate validator.
-}
module PartnerChains.Scripts.CommitteeCandidateValidator (
  -- * Committee candidate validator
  -- $committeeCandidateValidator
  mkCommitteeCandidateValidator,
  committeeCandidateValidatorUntyped,
  compiledValidator,
  serialisableValidator,
) where

import PartnerChains.Types (VersionedGenericDatum (..))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Data.V2 (PubKeyHash, ScriptContext, TxInfo, scriptContextTxInfo, serialiseCompiledCode)
import PlutusLedgerApi.V2.Data.Contexts (txSignedBy)
import PlutusTx qualified
import PlutusTx.Prelude

{- $committeeCandidateValidator

Validator storing committee candidate UTXOs. Each UTXO stores the `PubKeyHash` of
the candidate. Transaction must be signed by the candidate.

Error codes:

* ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01: Transaction not signed by the original submitter.
-}
{-# INLINEABLE mkCommitteeCandidateValidator #-}
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
