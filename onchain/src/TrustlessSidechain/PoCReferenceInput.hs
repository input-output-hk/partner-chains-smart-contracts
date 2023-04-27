{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- | A module for a trivial proof of concept (abbr. PoC) on chain script
 demonstrating the use of a reference input. In particular, we provide two
 scripts

    1. 'mkPoCToReferenceInputValidator': a script which always fails and holds an
    integer as a witness datum i.e., this holds the datum *to reference*

    2. 'mkPoCReferenceInputValidator': A script which checks if there exists exactly
    one script like 1. as a reference input, and verifies that the 1.'s witness
    datum is equal to 2.'s redeemer.

 This is used on the ctl side as a minimal example / test of using reference
 inputs.

 Since this is just used as a proof of concept on the ctl side, we have no
 offchain Haskell equivalent
-}
module TrustlessSidechain.PoCReferenceInput (
  mkPoCToReferenceInputValidator,
  mkPoCToReferenceInputValidatorUntyped,
  serialisablePoCToReferenceInputValidator,
  mkPoCReferenceInputValidator,
  mkPoCReferenceInputValidatorUntyped,
  serialisablePoCReferenceInputValidator,
) where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (UntypedValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Validators
import Plutus.V2.Ledger.Api (Address, Datum (getDatum), Script)
import Plutus.V2.Ledger.Api qualified as Api
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoReferenceInputs),
  TxOut (txOutAddress, txOutDatum),
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import Plutus.V2.Ledger.Tx (
  OutputDatum (OutputDatumHash),
 )
import PlutusTx qualified
import PlutusTx.Prelude

-- * To Reference

{- | 'mkPoCToReferenceInputValidator'
 A script which always errors, so this script cannot be spent. One potential
 use of this is for other scripts to read the datum of this script, when this
 script is given as a reference input.
-}
mkPoCToReferenceInputValidator :: Integer -> () -> ScriptContext -> Bool
mkPoCToReferenceInputValidator _dat _red _ctx =
  traceError "error 'mkPoCToReferenceInputValidator' attempt to spend"

-- | 'mkPoCToReferenceInputValidatorUntyped' is an untyped script of 'mkPoCToReferenceInputValidator'
mkPoCToReferenceInputValidatorUntyped :: UntypedValidator
mkPoCToReferenceInputValidatorUntyped =
  Validators.mkUntypedValidator mkPoCToReferenceInputValidator

{- | 'serialisablePoCToReferenceInputValidator' is a serialisable untyped script of
 'mkPoCToReferenceInputValidator'
-}
serialisablePoCToReferenceInputValidator :: Versioned Script
serialisablePoCToReferenceInputValidator =
  Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkPoCToReferenceInputValidatorUntyped||])) PlutusV2

-- * Reference

{- | 'mkPoCReferenceInputValidator'
 A script which verifies that the given 'Address' is a reference input AND the
 given 'Address''s witness datum is the redeemer.
-}
mkPoCReferenceInputValidator :: Address -> () -> Integer -> ScriptContext -> Bool
mkPoCReferenceInputValidator addr _dat red ctx
  | [txInInfo] <- filter ((addr ==) . txOutAddress . txInInfoResolved) $ txInfoReferenceInputs info =
    case () of
      _
        | OutputDatumHash dh <- txOutDatum (txInInfoResolved txInInfo)
          , Just d <- Contexts.findDatum dh info ->
          traceIfFalse
            "error 'mkPoCReferenceInputValidator': reference input and redeemer mismatch"
            $ PlutusTx.unsafeFromBuiltinData (getDatum d) == red
        | otherwise ->
          traceError "error 'mkPoCReferenceInputValidator': failed to get witness datum"
  | otherwise = traceError "error 'mkPoCReferenceInputValidator': not spending exactly one reference input of given address"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- | 'mkPoCReferenceInputValidatorUntyped' is an untyped script of 'mkPoCReferenceInputValidator'
mkPoCReferenceInputValidatorUntyped :: BuiltinData -> UntypedValidator
mkPoCReferenceInputValidatorUntyped =
  Validators.mkUntypedValidator . mkPoCReferenceInputValidator . PlutusTx.unsafeFromBuiltinData

{- | 'serialisablePoCReferenceInputValidator' is a serialisable untyped script of
 'mkPoCReferenceInputValidator'
-}
serialisablePoCReferenceInputValidator :: Versioned Script
serialisablePoCReferenceInputValidator =
  Versioned (Api.fromCompiledCode $$(PlutusTx.compile [||mkPoCReferenceInputValidatorUntyped||])) PlutusV2
