{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : TrustlessSidechain.AlwaysPassingScripts
--
-- This module provides Plutus scripts (validator and minting policy) that always succeed.
-- These scripts are useful for integration testing in mock chains, where validation
-- success needs to be ensured regardless of inputs.
module TrustlessSidechain.AlwaysPassingScripts (
  -- * Validator
  mkAlwaysPassingValidator,
  mkAlwaysPassingValidatorUntyped,
  serialisableAlwaysPassingValidator,

  -- * Minting Policy
  mkAlwaysPassingPolicy,
  mkAlwaysPassingPolicyUntyped,
  serialisableAlwaysPassingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Always-Passing Validator
--------------------------------------------------------------------------------

-- | A typed validator function that always passes.
--
-- All arguments  are ignored. The function always returns 'True'.
mkAlwaysPassingValidator ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Datum (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always returns True
  Bool
mkAlwaysPassingValidator _ _ _ _ = True

-- | An untyped version of 'mkAlwaysPassingValidator' conforming to the Plutus script interface.
--
-- Wraps the result in 'check', which succeeds when the result is 'True'.
mkAlwaysPassingValidatorUntyped ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Datum (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always succeeds
  BuiltinUnit
mkAlwaysPassingValidatorUntyped seed datum redeemer ctx =
  check
    $ mkAlwaysPassingValidator
      seed
      datum
      redeemer
      ctx

-- | A serialised version of the always-passing validator script.
serialisableAlwaysPassingValidator :: SerialisedScript
serialisableAlwaysPassingValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysPassingValidatorUntyped||])

--------------------------------------------------------------------------------
-- Always-Passing Minting Policy
--------------------------------------------------------------------------------

-- | A typed minting policy that always allows minting.
--
-- All arguments are ignored. The policy always returns 'True'.
{-# INLINEABLE mkAlwaysPassingPolicy #-}
mkAlwaysPassingPolicy ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always returns True
  Bool
mkAlwaysPassingPolicy _ _ _ = True

-- | An untyped version of 'mkAlwaysPassingPolicy', suitable for Plutus compilation.
--
-- Wraps the result in 'check', which passes when the result is 'True'.
{-# INLINEABLE mkAlwaysPassingPolicyUntyped #-}
mkAlwaysPassingPolicyUntyped ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always succeeds
  BuiltinUnit
mkAlwaysPassingPolicyUntyped seed redeemer ctx =
  check
    $ mkAlwaysPassingPolicy
      seed
      redeemer
      ctx

-- | A serialised version of the always-passing minting policy.
serialisableAlwaysPassingPolicy :: SerialisedScript
serialisableAlwaysPassingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysPassingPolicyUntyped||])
