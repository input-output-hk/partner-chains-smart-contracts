{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.AlwaysPassingScripts (
  mkAlwaysPassingValidator,
  mkAlwaysPassingValidatorUntyped,
  serialisableAlwaysPassingValidator,
  mkAlwaysPassingPolicy,
  mkAlwaysPassingPolicyUntyped,
  serialisableAlwaysPassingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude (
  Bool (True),
  BuiltinData,
  BuiltinUnit,
  check,
  ($),
 )

-- Always Passing Validator and Always Passing Minting Policy
-- are scripts that unconditionally pass. Such scripts are
-- useful in integration tests.

-- Both scripts are parametrized by an Integer. That allows for
-- obtaining different currency symbols.
mkAlwaysPassingValidator ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  Bool
mkAlwaysPassingValidator _ _ _ _ = True

mkAlwaysPassingValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkAlwaysPassingValidatorUntyped seed datum redeemer ctx =
  check
    $ mkAlwaysPassingValidator
      seed
      datum
      redeemer
      ctx

serialisableAlwaysPassingValidator :: SerialisedScript
serialisableAlwaysPassingValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysPassingValidatorUntyped||])

{-# INLINEABLE mkAlwaysPassingPolicy #-}
mkAlwaysPassingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
mkAlwaysPassingPolicy _ _ _ = True

{-# INLINEABLE mkAlwaysPassingPolicyUntyped #-}
mkAlwaysPassingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkAlwaysPassingPolicyUntyped seed redeemer ctx =
  check
    $ mkAlwaysPassingPolicy
      seed
      redeemer
      ctx

serialisableAlwaysPassingPolicy :: SerialisedScript
serialisableAlwaysPassingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysPassingPolicyUntyped||])
