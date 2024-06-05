{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.IlliquidCirculationSupply (
  mkIlliquidCirculationSupplyValidator,
  mkIlliquidCirculationSupplyValidatorUntyped,
  serialisableIlliquidCirculationSupplyValidator,
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
  IlliquidCirculationSupplyRedeemer,
 )
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Versioning (
  VersionOracleConfig,
 )

mkIlliquidCirculationSupplyValidator ::
  VersionOracleConfig ->
  () ->
  IlliquidCirculationSupplyRedeemer ->
  Unsafe.ScriptContext ->
  Bool
mkIlliquidCirculationSupplyValidator _ _ _ _ = True

mkIlliquidCirculationSupplyValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkIlliquidCirculationSupplyValidatorUntyped voc rd rr ctx =
  check $
    mkIlliquidCirculationSupplyValidator
      (PlutusTx.unsafeFromBuiltinData voc)
      (PlutusTx.unsafeFromBuiltinData rd)
      (PlutusTx.unsafeFromBuiltinData rr)
      (Unsafe.ScriptContext ctx)

serialisableIlliquidCirculationSupplyValidator :: Script
serialisableIlliquidCirculationSupplyValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkIlliquidCirculationSupplyValidatorUntyped||])
