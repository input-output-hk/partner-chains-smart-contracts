{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.PoCECDSA (
  ECDSARed (..),
  serialisableValidator,
) where

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  mkUntypedValidator,
 )

data ECDSARed = ECDSARed
  { msg :: BuiltinByteString
  , sig :: BuiltinByteString
  , pk :: BuiltinByteString
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Show)

PlutusTx.unstableMakeIsData ''ECDSARed

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> ECDSARed -> ScriptContext -> Bool
mkValidator _ (ECDSARed msg' sig' pk') _ =
  traceIfFalse "signature check failed"
    . verifyEcdsaSecp256k1Signature pk' msg'
    $ sig'

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator = mkUntypedValidator mkValidator

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||untypedValidator||])
