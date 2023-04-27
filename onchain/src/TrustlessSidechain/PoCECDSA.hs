{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TrustlessSidechain.PoCECDSA (
  ECDSARed (..),
  serialisableValidator,
) where

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Script,
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified

data ECDSARed = ECDSARed
  { msg :: BuiltinByteString
  , sig :: BuiltinByteString
  , pk :: BuiltinByteString
  }
  deriving stock (Prelude.Eq, Prelude.Show)

PlutusTx.unstableMakeIsData ''ECDSARed

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> ECDSARed -> V2.ScriptContext -> Bool
mkValidator _ (ECDSARed msg' sig' pk') _ =
  traceIfFalse "signature check failed"
    . verifyEcdsaSecp256k1Signature pk' msg'
    $ sig'

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator = ScriptUtils.mkUntypedValidator mkValidator

serialisableValidator :: Versioned Script
serialisableValidator =
  Versioned (fromCompiledCode $$(PlutusTx.compile [||untypedValidator||])) PlutusV2
