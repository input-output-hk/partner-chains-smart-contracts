{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.PoCAlwaysPassingScripts (
  mkAlwaysPassingValidator,
  mkAlwaysPassingValidatorUntyped,
  serialisableAlwaysPassingValidator,
  mkAlwaysPassingPolicy,
  mkAlwaysPassingPolicyUntyped,
  serialisableAlwaysPassingPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude (
  Bool (True),
  BuiltinData,
  Integer,
  check,
  ($),
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

-- Always Passing Validator and Always Passing Minting Policy
-- are scripts that unconditionally pass. Such scripts are
-- useful in integration tests.

-- Both scripts are parametrized by an Integer. That allows for
-- obtaining different currency symbols.
mkAlwaysPassingValidator ::
  Integer ->
  BuiltinData ->
  BuiltinData ->
  Unsafe.ScriptContext ->
  Bool
mkAlwaysPassingValidator _ _ _ _ = True

mkAlwaysPassingValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysPassingValidatorUntyped seed datum redeemer ctx =
  check $
    mkAlwaysPassingValidator
      (PlutusTx.unsafeFromBuiltinData seed)
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableAlwaysPassingValidator :: Script
serialisableAlwaysPassingValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkAlwaysPassingValidatorUntyped||])

{-# INLINEABLE mkAlwaysPassingPolicy #-}
mkAlwaysPassingPolicy :: Integer -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkAlwaysPassingPolicy _ _ _ = True

{-# INLINEABLE mkAlwaysPassingPolicyUntyped #-}
mkAlwaysPassingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysPassingPolicyUntyped seed redeemer ctx =
  check $
    mkAlwaysPassingPolicy
      (PlutusTx.unsafeFromBuiltinData seed)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableAlwaysPassingPolicy :: Script
serialisableAlwaysPassingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkAlwaysPassingPolicyUntyped||])
