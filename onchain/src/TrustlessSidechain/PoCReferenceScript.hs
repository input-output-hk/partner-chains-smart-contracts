{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- | A module for a trivial proof of concept (abbr. PoC) on chain script
 demonstrating the use of a reference script. In particular, we provide two
 scripts

    1. 'mkPoCToReferenceScriptValidator': a script which always succeeds.

    2. 'mkPoCReferenceScriptValidator': A script if there is an input which
    has a reference script as given in its redeemer.

 This is used on the ctl side as a minimal example / test of using reference
 scripts.

 Since this is just used as a proof of concept on the ctl side, we have no
 offchain Haskell equivalent
-}
module TrustlessSidechain.PoCReferenceScript (
  mkPoCToReferenceScriptValidator,
  mkPoCToReferenceScriptValidatorUntyped,
  serialisablePoCToReferenceScriptValidator,
  mkPoCReferenceScriptValidator,
  mkPoCReferenceScriptValidatorUntyped,
  serialisablePoCReferenceScriptValidator,
) where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import Plutus.V2.Ledger.Api (Script, ScriptHash)
import Plutus.V2.Ledger.Api qualified as Api
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs),
  TxOut (txOutReferenceScript),
 )
import PlutusTx qualified
import PlutusTx.Prelude

-- * To Reference

{- | 'mkPoCToReferenceScriptValidator'
 A script which is always true.
-}
mkPoCToReferenceScriptValidator :: () -> () -> ScriptContext -> Bool
mkPoCToReferenceScriptValidator _dat _red _ctx = True

-- | 'mkPoCToReferenceScriptValidatorUntyped' is an untyped script of 'mkPoCToReferenceScriptValidator'
mkPoCToReferenceScriptValidatorUntyped :: UntypedValidator
mkPoCToReferenceScriptValidatorUntyped = Validators.mkUntypedValidator mkPoCToReferenceScriptValidator

{- | 'serialisablePoCToReferenceScriptValidator' is a serialisable untyped script of
 'mkPoCToReferenceScriptValidator'
-}
serialisablePoCToReferenceScriptValidator :: Versioned Script
serialisablePoCToReferenceScriptValidator = Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkPoCToReferenceScriptValidatorUntyped||])) PlutusV2

-- * Reference

{- | 'mkPoCReferenceScriptValidator'
 A script which verifies that the given 'Address' is a reference input AND the
 given 'Address''s witness datum is the redeemer.
-}
mkPoCReferenceScriptValidator :: () -> ScriptHash -> ScriptContext -> Bool
mkPoCReferenceScriptValidator _dat red ctx =
  traceIfFalse "error 'mkPoCReferenceScriptValidator': no tx with given reference script's script hash" $
    any (\txin -> txOutReferenceScript (txInInfoResolved txin) == Just red) $
      txInfoInputs info
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- | 'mkPoCReferenceScriptValidatorUntyped' is an untyped script of 'mkPoCReferenceScriptValidator'
mkPoCReferenceScriptValidatorUntyped :: UntypedValidator
mkPoCReferenceScriptValidatorUntyped = Validators.mkUntypedValidator mkPoCReferenceScriptValidator

{- | 'serialisablePoCReferenceScriptValidator' is a serialisable untyped script of
 'mkPoCReferenceScriptValidator'
-}
serialisablePoCReferenceScriptValidator :: Versioned Script
serialisablePoCReferenceScriptValidator = Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkPoCReferenceScriptValidatorUntyped||])) PlutusV2
