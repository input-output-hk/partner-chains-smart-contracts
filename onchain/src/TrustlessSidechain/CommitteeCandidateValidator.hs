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

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V2 (PubKeyHash, serialiseCompiledCode)
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (VersionedGenericDatum (..))
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

{-# INLINEABLE mkCommitteeCandidateValidator #-}
-- OnChain error descriptions:
--
--   ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01: Transaction not signed by the
--   original submitter.
mkCommitteeCandidateValidator ::
  BuiltinData ->
  VersionedGenericDatum PubKeyHash ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkCommitteeCandidateValidator _sidechainParams (VersionedGenericDatum {datum = pkh}) _redeemer ctx =
  traceIfFalse "ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01" isSigned
  where
    info :: Unsafe.TxInfo
    info = Unsafe.scriptContextTxInfo ctx
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
  check
    $ mkCommitteeCandidateValidator
      sidechainParams
      (unsafeFromBuiltinData datum)
      red
      (Unsafe.ScriptContext ctx)

serialisableValidator :: SerialisedScript
serialisableValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])
