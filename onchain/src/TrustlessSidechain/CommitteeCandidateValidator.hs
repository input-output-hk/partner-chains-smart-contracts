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

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext),
  txSignedBy,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (mkUntypedValidator)
import TrustlessSidechain.Types (
  BlockProducerRegistration (ownPkh),
  SidechainParams,
 )

{-# INLINEABLE mkCommitteeCandidateValidator #-}
mkCommitteeCandidateValidator ::
  SidechainParams ->
  BlockProducerRegistration ->
  () ->
  ScriptContext ->
  Bool
mkCommitteeCandidateValidator _ datum _ (ScriptContext txInfo _) =
  traceIfFalse "Must be signed by the original submitter" isSigned
  where
    isSigned :: Bool
    isSigned = txSignedBy txInfo (ownPkh datum)

{-# INLINEABLE committeeCandidateValidatorUntyped #-}
committeeCandidateValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
committeeCandidateValidatorUntyped =
  mkUntypedValidator
    . mkCommitteeCandidateValidator
    . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])
