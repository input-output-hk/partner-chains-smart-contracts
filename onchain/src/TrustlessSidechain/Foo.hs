{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Foo (
  mkFooPolicy,
  mkFooPolicyUntyped,
  serialisableFooPolicy,
) where

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

mkFooPolicy ::
  BuiltinData ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkFooPolicy _ _ _ = True

mkFooPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkFooPolicyUntyped d r scriptContext =
  check
    $ mkFooPolicy
      d
      r
      (Unsafe.wrap scriptContext)

serialisableFooPolicy :: Script
serialisableFooPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkFooPolicyUntyped||])
