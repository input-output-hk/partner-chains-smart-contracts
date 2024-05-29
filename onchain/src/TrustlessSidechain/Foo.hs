{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Foo (
  mkFooPolicy,
  mkFooPolicyUntyped,
  serialisableFooPolicy,
) where

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
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

serialisableFooPolicy :: Script
serialisableFooPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkFooPolicyUntyped||])
