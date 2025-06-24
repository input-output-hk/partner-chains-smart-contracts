{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.AlwaysFailingScripts (
  mkAlwaysFailingValidator,
  mkAlwaysFailingValidatorUntyped,
  serialisableAlwaysFailingValidator,
  mkAlwaysFailingPolicy,
  mkAlwaysFailingPolicyUntyped,
  serialisableAlwaysFailingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude

-- Always Failing Validator and Always Failing Minting Policy
-- are scripts that unconditionally fail. Such scripts are
-- useful in integration tests.

-- Both scripts are parametrized by an Integer. That allows for
-- obtaining different currency symbols.
mkAlwaysFailingValidator ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  Bool
mkAlwaysFailingValidator _ _ _ _ = False

mkAlwaysFailingValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkAlwaysFailingValidatorUntyped seed datum redeemer ctx =
  check $ mkAlwaysFailingValidator seed datum redeemer ctx

serialisableAlwaysFailingValidator :: SerialisedScript
serialisableAlwaysFailingValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailingValidatorUntyped||])

{-# INLINEABLE mkAlwaysFailingPolicy #-}
mkAlwaysFailingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
mkAlwaysFailingPolicy _ _ _ = False

{-# INLINEABLE mkAlwaysFailingPolicyUntyped #-}
mkAlwaysFailingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkAlwaysFailingPolicyUntyped seed redeemer ctx =
  check $ mkAlwaysFailingPolicy seed redeemer ctx

serialisableAlwaysFailingPolicy :: SerialisedScript
serialisableAlwaysFailingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailingPolicyUntyped||])
