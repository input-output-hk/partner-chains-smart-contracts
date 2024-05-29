{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Foo (
  mkFooPolicy,
  mkFooPolicyUntyped,
  serialisableFooPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude

mkFooPolicy ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
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
      scriptContext

serialisableFooPolicy :: SerialisedScript
serialisableFooPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkFooPolicyUntyped||])
