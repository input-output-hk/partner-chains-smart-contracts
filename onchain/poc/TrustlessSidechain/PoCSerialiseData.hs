{-# LANGUAGE TemplateHaskell #-}

{- | A module for a trivial proof of concept (abbr. PoC) on chain script
 demonstrating the use of the new builtin function
 'PlutusTx.Builtins.serialiseData'.

 This is used on the ctl side as a minimal example / test of using inline
 datums.

 Since this is just used as a proof of concept on the ctl side, we have no
 offchain Haskell equivalent
-}
module TrustlessSidechain.PoCSerialiseData (
  mkPoCSerialiseData,
  serialisablePoCSerialiseData,
) where

import Plutus.V2.Ledger.Api (Script, ScriptContext, fromCompiledCode)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import TrustlessSidechain.Utils (
  mkUntypedValidator,
 )

{- | 'mkPoCSerialiseData' is a validator script which succeeds iff the datum
 BuiltinByteString is the same as the @'PlutusTx.Builtins.serialiseData'
 redeemer@
-}
mkPoCSerialiseData :: BuiltinByteString -> BuiltinData -> ScriptContext -> Bool
mkPoCSerialiseData dat red _cxt = dat == Builtins.serialiseData red

{- | 'mkPoCSerialiseDataUntyped' is an untyped script of
 'mkPoCSerialiseData'
-}
mkPoCSerialiseDataUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPoCSerialiseDataUntyped = mkUntypedValidator mkPoCSerialiseData

{- | 'serialisablePoCSerialiseData' is a serialisable untyped script of
 'mkPoCSerialiseData'
-}
serialisablePoCSerialiseData :: Script
serialisablePoCSerialiseData = fromCompiledCode $$(PlutusTx.compile [||mkPoCSerialiseDataUntyped||])
