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
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

{-# INLINEABLE mkCommitteeCandidateValidator #-}
-- OnChain error descriptions:
--
--   ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01: Transaction not signed by the
--   original submitter.
mkCommitteeCandidateValidator ::
  BuiltinData ->
  Unsafe.BlockProducerRegistration ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkCommitteeCandidateValidator _sidechainParams datum _redeemer ctx =
  traceIfFalse "ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01" isSigned
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx
    pkh :: PubKeyHash
    pkh = Unsafe.decode $ Unsafe.ownPkh datum
    isSigned :: Bool
    isSigned = Unsafe.txSignedBy info pkh

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
      sidechainParams
      (Unsafe.BlockProducerRegistration datum)
      red
      (Unsafe.ScriptContext ctx)

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])
