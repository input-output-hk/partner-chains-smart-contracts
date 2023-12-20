{-# LANGUAGE TemplateHaskell #-}

-- | A module for a trivial proof of concept (abbr. PoC) on chain script
-- demonstrating the use of inline datum. In particular, we provide a script
-- succeeds iff its inline datum is its redeemer.
--
-- This is used on the ctl side as a minimal example / test of using inline
-- datums.
--
-- Since this is just used as a proof of concept on the ctl side, we have no
-- offchain Haskell equivalent
module TrustlessSidechain.PoCInlineDatum (
  mkPoCInlineDatumValidator,
  serialisablePoCInlineDatumValidator,
) where

import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  Script,
  ScriptContext,
  TxInInfo (txInInfoResolved),
  TxOut (txOutDatum),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  mkUntypedValidator,
 )

-- | 'mkPoCInlineDatumValidator'
-- A script which verifies that its inline datum is the redeemer.
mkPoCInlineDatumValidator :: Integer -> Integer -> ScriptContext -> Bool
mkPoCInlineDatumValidator _dat red ctx =
  case Contexts.findOwnInput ctx of
    Just ownTxInInfo ->
      let ownTxOut = txInInfoResolved ownTxInInfo
       in case txOutDatum ownTxOut of
            NoOutputDatum -> traceError "error 'mkPoCInlineDatum': unexpected 'NoOutputDatum'"
            OutputDatumHash _ -> traceError "error 'mkPoCInlineDatum': unexpected 'OutputDatumHash'"
            OutputDatum dat
              | dat' <- getDatum dat
                , Just i <- IsData.fromBuiltinData dat' ->
                traceIfFalse "error 'mkPoCInlineDatum': redeemer and datum mismatch" $ i == red
              | otherwise -> traceError "error 'mkPoCInlineDatum': 'fromBuiltinData' failed"
    Nothing -> traceError "error 'mkPoCInlineDatum': 'findOwnInput' failed"

-- | 'mkPoCInlineDatumValidatorUntyped' is an untyped script of 'mkPoCInlineDatumValidator'
mkPoCInlineDatumValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPoCInlineDatumValidatorUntyped = mkUntypedValidator mkPoCInlineDatumValidator

-- | 'serialisablePoCInlineDatumValidator' is a serialisable untyped script of
-- 'mkPoCInlineDatumValidator'
serialisablePoCInlineDatumValidator :: Script
serialisablePoCInlineDatumValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkPoCInlineDatumValidatorUntyped||])
