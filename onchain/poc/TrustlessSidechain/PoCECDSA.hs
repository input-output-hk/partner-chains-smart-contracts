{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.PoCECDSA (
  ECDSARed (..),
  serialisableValidator,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2 (ScriptContext)
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

serialisableValidator :: SerialisedScript
serialisableValidator =
  serialiseCompiledCode
    $$(PlutusTx.compile [||untypedValidator||])
