{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Foo (
  mkFooPolicy,
  mkFooPolicyUntyped,
  serialisableFooPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
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

serialisableFooPolicy :: SerialisedScript
serialisableFooPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkFooPolicyUntyped||])
