{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TrustlessSidechain.CommitteeCandidateValidator (
  mkCommitteeCandidateValidator,
  committeeCandidateValidatorUntyped,
  serialisableValidator,
) where

import Plutus.V2.Ledger.Api (PubKeyHash, Script, fromCompiledCode)
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  BlockProducerRegistration,
  SidechainParams,
 )
import TrustlessSidechain.TypesRaw qualified as Raw

{-# INLINEABLE mkCommitteeCandidateValidator #-}
-- OnChain error descriptions:
--
--   ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01: Transaction not signed by the
--   original submitter.
mkCommitteeCandidateValidator ::
  SidechainParams ->
  BlockProducerRegistration ->
  BuiltinData ->
  Raw.ScriptContext ->
  Bool
mkCommitteeCandidateValidator _sidechainParams datum _redeemer ctx =
  traceIfFalse "ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01" isSigned
  where
    info :: Raw.TxInfo
    info = Raw.scriptContextTxInfo ctx
    pkh :: PubKeyHash
    pkh = get @"ownPkh" datum
    isSigned :: Bool
    isSigned = Raw.txSignedBy info pkh

{-# INLINEABLE committeeCandidateValidatorUntyped #-}
committeeCandidateValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
committeeCandidateValidatorUntyped sidechainParams datum red ctx =
  check $
    mkCommitteeCandidateValidator
      (PlutusTx.unsafeFromBuiltinData sidechainParams)
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData red)
      (Raw.ScriptContext ctx)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])
