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
import TrustlessSidechain.PlutusPrelude (
  Bool (False),
  BuiltinData,
  Integer,
  check,
  ($),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

-- Always Failing Validator and Always Failing Minting Policy
-- are scripts that unconditionally fail. Such scripts are
-- useful in integration tests.

-- Both scripts are parametrized by an Integer. That allows for
-- obtaining different currency symbols.
mkAlwaysFailingValidator ::
  Integer ->
  BuiltinData ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkAlwaysFailingValidator _ _ _ _ = False

mkAlwaysFailingValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysFailingValidatorUntyped seed datum redeemer ctx =
  check
    $ mkAlwaysFailingValidator
      (PlutusTx.unsafeFromBuiltinData seed)
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableAlwaysFailingValidator :: SerialisedScript
serialisableAlwaysFailingValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailingValidatorUntyped||])

{-# INLINEABLE mkAlwaysFailingPolicy #-}
mkAlwaysFailingPolicy :: Integer -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkAlwaysFailingPolicy _ _ _ = False

{-# INLINEABLE mkAlwaysFailingPolicyUntyped #-}
mkAlwaysFailingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysFailingPolicyUntyped seed redeemer ctx =
  check
    $ mkAlwaysFailingPolicy
      (PlutusTx.unsafeFromBuiltinData seed)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableAlwaysFailingPolicy :: SerialisedScript
serialisableAlwaysFailingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailingPolicyUntyped||])
