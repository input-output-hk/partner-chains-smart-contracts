{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.ExampleVFunction (
  mkVFunctionPolicy,
  mkVFunctionPolicyUntyped,
  serialisableVFunctionPolicy,
) where

import GHC.Num (fromInteger)
import PlutusLedgerApi.V1.Value (currencySymbolValueOf)
import PlutusLedgerApi.V2 (
  Extended (Finite),
  LowerBound (LowerBound),
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  SerialisedScript,
  getPOSIXTime,
  ivFrom,
  scriptContextPurpose,
  scriptContextTxInfo,
  serialiseCompiledCode,
  txInfoMint,
  txInfoValidRange,
 )
import PlutusTx qualified
import PlutusTx.Builtins (divideInteger)
import PlutusTx.Prelude hiding (fromInteger)

{-# INLINEABLE mkVFunctionPolicy #-}
mkVFunctionPolicy :: Integer -> BuiltinData -> ScriptContext -> Bool
mkVFunctionPolicy time _ (ScriptContext {scriptContextPurpose = Minting cs, scriptContextTxInfo}) =
  minted <= allowedMint
  where
    validityIntervalStart = case ivFrom $ txInfoValidRange scriptContextTxInfo of
      LowerBound (Finite a) True -> getPOSIXTime a
      _ -> traceError "invalid validity interval"

    cycleLength = fromInteger (60 * 1000) -- 1 token per minute
    allowedMint = (validityIntervalStart - time) `divideInteger` cycleLength

    minted = currencySymbolValueOf (txInfoMint scriptContextTxInfo) cs
mkVFunctionPolicy _ _ _ = False

{-# INLINEABLE mkVFunctionPolicyUntyped #-}
mkVFunctionPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
mkVFunctionPolicyUntyped posixTime redeemer ctx =
  check
    $ mkVFunctionPolicy
      (PlutusTx.unsafeFromBuiltinData posixTime)
      redeemer
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableVFunctionPolicy :: SerialisedScript
serialisableVFunctionPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVFunctionPolicyUntyped||])
