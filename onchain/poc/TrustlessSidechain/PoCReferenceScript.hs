{-# LANGUAGE TemplateHaskell #-}

-- | A module for a trivial proof of concept (abbr. PoC) on chain script
-- demonstrating the use of a reference script. In particular, we provide two
-- scripts
--
--    1. 'mkPoCToReferenceScriptValidator': a script which always succeeds.
--
--    2. 'mkPoCReferenceScriptValidator': A script if there is an input which
--    has a reference script as given in its redeemer.
--
-- This is used on the ctl side as a minimal example / test of using reference
-- scripts.
--
-- Since this is just used as a proof of concept on the ctl side, we have no
-- offchain Haskell equivalent
module TrustlessSidechain.PoCReferenceScript (
  mkPoCToReferenceScriptValidator,
  serialisablePoCToReferenceScriptValidator,
  mkPoCReferenceScriptValidator,
  serialisablePoCReferenceScriptValidator,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2 (
  ScriptContext (scriptContextTxInfo),
  ScriptHash,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs),
  TxOut (txOutReferenceScript)
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  mkUntypedValidator,
 )

-- * To Reference

-- | 'mkPoCToReferenceScriptValidator'
-- A script which is always true.
mkPoCToReferenceScriptValidator :: () -> () -> ScriptContext -> Bool
mkPoCToReferenceScriptValidator _dat _red _ctx = True

-- | 'mkPoCToReferenceScriptValidatorUntyped' is an untyped script of 'mkPoCToReferenceScriptValidator'
mkPoCToReferenceScriptValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPoCToReferenceScriptValidatorUntyped = mkUntypedValidator mkPoCToReferenceScriptValidator

-- | 'serialisablePoCToReferenceScriptValidator' is a serialisable untyped script of
-- 'mkPoCToReferenceScriptValidator'
serialisablePoCToReferenceScriptValidator :: SerialisedScript
serialisablePoCToReferenceScriptValidator = serialiseCompiledCode $$(PlutusTx.compile [||mkPoCToReferenceScriptValidatorUntyped||])

-- * Reference

-- | 'mkPoCReferenceScriptValidator'
-- A script which verifies that the given 'Address' is a reference input AND the
-- given 'Address''s witness datum is the redeemer.
mkPoCReferenceScriptValidator :: () -> ScriptHash -> ScriptContext -> Bool
mkPoCReferenceScriptValidator _dat red ctx =
  traceIfFalse "error 'mkPoCReferenceScriptValidator': no tx with given reference script's script hash" $
    any (\txin -> txOutReferenceScript (txInInfoResolved txin) == Just red) $
      txInfoInputs info
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- | 'mkPoCReferenceScriptValidatorUntyped' is an untyped script of 'mkPoCReferenceScriptValidator'
mkPoCReferenceScriptValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPoCReferenceScriptValidatorUntyped = mkUntypedValidator mkPoCReferenceScriptValidator

-- | 'serialisablePoCReferenceScriptValidator' is a serialisable untyped script of
-- 'mkPoCReferenceScriptValidator'
serialisablePoCReferenceScriptValidator :: SerialisedScript
serialisablePoCReferenceScriptValidator = serialiseCompiledCode $$(PlutusTx.compile [||mkPoCReferenceScriptValidatorUntyped||])
