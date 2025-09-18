{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : PartnerChains.Scripts.AlwaysFailingScripts
Description : Always-failing Plutus scripts for integration testing

This module defines always-failing Plutus validator and minting policy scripts.
These are useful for testing scenarios where script validation must fail deliberately.
-}
module PartnerChains.Scripts.AlwaysFailingScripts (
  -- * Validator
  mkAlwaysFailingValidator,
  mkAlwaysFailingValidatorUntyped,
  serialisableAlwaysFailingValidator,

  -- * MintingPolicy
  mkAlwaysFailingPolicy,
  mkAlwaysFailingPolicyUntyped,
  serialisableAlwaysFailingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import PlutusTx qualified
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Always-Failing Validator
--------------------------------------------------------------------------------

{- | A typed validator function that always fails.

All arguments are ignored. The function always returns False. Intended for testing validation
failure paths.
-}
mkAlwaysFailingValidator ::
  -- | Arbitrary seed (ignored)
  BuiltinData ->
  -- | Datum (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always returns false
  Bool
mkAlwaysFailingValidator _ _ _ _ = False

{- | An untyped version of 'mkAlwaysFailingValidator' that conforms to the Plutus
script interface.
-}
mkAlwaysFailingValidatorUntyped ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Datum (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always fails via 'check'
  BuiltinUnit
mkAlwaysFailingValidatorUntyped seed datum redeemer ctx =
  check $ mkAlwaysFailingValidator seed datum redeemer ctx

-- | A compiled and serialised version of the always-failing validator script.
serialisableAlwaysFailingValidator :: SerialisedScript
serialisableAlwaysFailingValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailingValidatorUntyped||])

--------------------------------------------------------------------------------
-- Always-Failing Minting Policy
--------------------------------------------------------------------------------

{- | A typed minting policy function that always fails.

All arguments (a parameter, redeemer, and script context) are ignored.
Always returns 'False', causing the minting policy to fail.

Useful for testing failure conditions in minting transactions.
-}
{-# INLINEABLE mkAlwaysFailingPolicy #-}
mkAlwaysFailingPolicy ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always returns False
  Bool
mkAlwaysFailingPolicy _ _ _ = False

{- | An untyped version of 'mkAlwaysFailingPolicy', suitable for Plutus script compilation.

Wraps the result with 'check' to throw on 'False'.
-}
{-# INLINEABLE mkAlwaysFailingPolicyUntyped #-}
mkAlwaysFailingPolicyUntyped ::
  -- | Arbitrary seed/parameter (ignored)
  BuiltinData ->
  -- | Redeemer (ignored)
  BuiltinData ->
  -- | Script context (ignored)
  BuiltinData ->
  -- | Always fails via 'check'
  BuiltinUnit
mkAlwaysFailingPolicyUntyped seed redeemer ctx =
  check $ mkAlwaysFailingPolicy seed redeemer ctx

{- | A compiled and serialised version of the always-failing minting policy.

Useful for producing minting policies in tests where validation is expected to fail.
-}
serialisableAlwaysFailingPolicy :: SerialisedScript
serialisableAlwaysFailingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailingPolicyUntyped||])
