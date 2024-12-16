{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.ExampleVFunction (
  mkVFunctionPolicy,
  mkVFunctionPolicyUntyped,
  serialisableVFunctionPolicy,
) where

import PlutusLedgerApi.V1.Value (currencySymbolValueOf)
import PlutusLedgerApi.V2 (
  Extended (Finite),
  LowerBound (LowerBound),
  POSIXTime,
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
import TrustlessSidechain.PlutusPrelude (
  Bool (False, True),
  BuiltinData,
  check,
  div,
  fromInteger,
  fromString,
  traceError,
  ($),
  (*),
  (-),
  (<=),
 )

-- import Plutus.V1.Ledger.Time (POSIXTime, getPOSIXTime)

{-# INLINEABLE mkVFunctionPolicy #-}
mkVFunctionPolicy :: POSIXTime -> BuiltinData -> ScriptContext -> Bool
mkVFunctionPolicy time _ (ScriptContext {scriptContextPurpose = Minting cs, scriptContextTxInfo}) =
  minted <= allowedMint
  where
    valididyIntervalStart = case ivFrom $ txInfoValidRange scriptContextTxInfo of
      LowerBound (Finite a) True -> a
      _ -> traceError "invalid validity interval"

    cycleLength = fromInteger (60 * 1) -- 60*60*24
    allowedMint = getPOSIXTime ((valididyIntervalStart - time) `div` cycleLength)

    minted = currencySymbolValueOf (txInfoMint scriptContextTxInfo) cs
mkVFunctionPolicy _ _ _ = False

{-# INLINEABLE mkVFunctionPolicyUntyped #-}
mkVFunctionPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkVFunctionPolicyUntyped posixTime redeemer ctx =
  check
    $ mkVFunctionPolicy
      (PlutusTx.unsafeFromBuiltinData posixTime)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableVFunctionPolicy :: SerialisedScript
serialisableVFunctionPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkVFunctionPolicyUntyped||])
