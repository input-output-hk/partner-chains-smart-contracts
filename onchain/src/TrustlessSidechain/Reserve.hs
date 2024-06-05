{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Reserve (
  mkReserveValidator,
  mkReserveValidatorUntyped,
  serialisableReserveValidator,
  mkReserveAuthPolicy,
  mkReserveAuthPolicyUntyped,
  serialisableReserveAuthPolicy,
) where

import Plutus.V2.Ledger.Api (
  Script,
  fromCompiledCode,
 )
import PlutusTx qualified
import TrustlessSidechain.PlutusPrelude (
  Bool (True),
  BuiltinData,
  check,
  ($),
 )
import TrustlessSidechain.Types (
  ReserveDatum,
  ReserveRedeemer,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Versioning (
  VersionOracleConfig,
 )

mkReserveValidator ::
  VersionOracleConfig ->
  ReserveDatum ->
  ReserveRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkReserveValidator _ _ _ _ = True

mkReserveValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkReserveValidatorUntyped voc rd rr ctx =
  check $
    mkReserveValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (Unsafe.ScriptContext ctx)

serialisableReserveValidator :: Script
serialisableReserveValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkReserveValidatorUntyped||])

{-# INLINEABLE mkReserveAuthPolicy #-}
mkReserveAuthPolicy :: VersionOracleConfig -> BuiltinData -> Unsafe.ScriptContext -> Bool
mkReserveAuthPolicy _ _ _ = True

{-# INLINEABLE mkReserveAuthPolicyUntyped #-}
mkReserveAuthPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkReserveAuthPolicyUntyped voc red ctx =
  check $
    mkReserveAuthPolicy
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData red)
      (Unsafe.ScriptContext ctx)

serialisableReserveAuthPolicy :: Script
serialisableReserveAuthPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkReserveAuthPolicyUntyped||])
