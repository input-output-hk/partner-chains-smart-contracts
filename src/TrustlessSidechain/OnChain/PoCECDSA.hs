{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TrustlessSidechain.OnChain.PoCECDSA (
  ecdsaVerifyScript,
  ecdsaVerifyScriptShortBs,
  ecdsaVerifyScriptHash,
  ecdsaVerifyAddress,
  serialisableValidator,
  ECDSARed (..),
) where

import PlutusTx.Prelude

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as TypedScripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as ScriptUtils
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V2.Ledger.Api (
  Address,
  Script,
  Validator,
  ValidatorHash,
  fromCompiledCode,
  mkValidatorScript,
  unValidatorScript,
 )
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import Prelude qualified

data ECDSARed = ECDSARed
  { msg :: BuiltinByteString
  , sig :: BuiltinByteString
  , pk :: BuiltinByteString
  }
  deriving (Prelude.Eq, Prelude.Show)

PlutusTx.unstableMakeIsData ''ECDSARed

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> ECDSARed -> V2.ScriptContext -> Bool
mkValidator _ (ECDSARed msg' sig' pk') _ =
  traceIfFalse "signature check failed" (verifyEcdsaSecp256k1Signature pk' msg' sig')

validator :: Validator
validator =
  mkValidatorScript
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.mkUntypedValidator mkValidator

script :: Script
script = unValidatorScript validator

ecdsaVerifyScriptShortBs :: SBS.ShortByteString
ecdsaVerifyScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

ecdsaVerifyScript :: PlutusScript PlutusScriptV2
ecdsaVerifyScript = PlutusScriptSerialised ecdsaVerifyScriptShortBs

ecdsaVerifyScriptHash :: ValidatorHash
ecdsaVerifyScriptHash = Scripts.validatorHash validator

ecdsaVerifyAddress :: Address
ecdsaVerifyAddress = scriptHashAddress ecdsaVerifyScriptHash

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator = ScriptUtils.mkUntypedValidator mkValidator

serialisableValidator :: Script
serialisableValidator = fromCompiledCode $$(PlutusTx.compile [||untypedValidator||])
