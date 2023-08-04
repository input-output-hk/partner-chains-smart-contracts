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

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as ScriptUtils
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
    pkh :: Ledger.PubKeyHash
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
  ScriptUtils.mkUntypedValidator
    . mkCommitteeCandidateValidator
    . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Versioned Ledger.Script
serialisableValidator =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])) PlutusV2
