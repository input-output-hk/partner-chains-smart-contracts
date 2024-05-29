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
  Bool
mkFooPolicy _ _ = True

mkFooPolicyUntyped ::
  BuiltinData ->
  BuiltinData ->
  ()
mkFooPolicyUntyped r scriptContext =
  check
    $ mkFooPolicy
      r
      scriptContext

serialisableFooPolicy :: SerialisedScript
serialisableFooPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkFooPolicyUntyped||])
