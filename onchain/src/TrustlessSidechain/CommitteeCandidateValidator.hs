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
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInfo,
  txSignedBy,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  BlockProducerRegistration,
  SidechainParams,
 )
import TrustlessSidechain.Utils (mkUntypedValidator)

{-# INLINEABLE mkCommitteeCandidateValidator #-}
mkCommitteeCandidateValidator ::
  SidechainParams ->
  BlockProducerRegistration ->
  () ->
  ScriptContext ->
  Bool
mkCommitteeCandidateValidator _ datum _ ctx =
  traceIfFalse "Must be signed by the original submitter" isSigned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    pkh :: PubKeyHash
    pkh = get @"ownPkh" datum
    isSigned :: Bool
    isSigned = txSignedBy info pkh

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
