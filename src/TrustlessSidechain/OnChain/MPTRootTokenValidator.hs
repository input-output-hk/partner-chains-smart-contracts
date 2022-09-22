{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator where

import Ledger qualified
import Plutus.Script.Utils.V2.Scripts (Validator, validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (PassiveBrdgSidechainParams)

{-# INLINEABLE mkValidator #-}
mkValidator :: PassiveBrdgSidechainParams -> () -> () -> ScriptContext -> Bool
mkValidator _ () () _ = False

validator :: PassiveBrdgSidechainParams -> Validator
validator p =
  Ledger.mkValidatorScript
    ($$(compile [||untypedValidator||]) `applyCode` liftCode p)
  where
    untypedValidator = ScriptUtils.mkUntypedValidator . mkValidator

hash :: PassiveBrdgSidechainParams -> Ledger.ValidatorHash
hash = validatorHash . validator

address :: PassiveBrdgSidechainParams -> Ledger.Address
address = Ledger.scriptHashAddress . hash

-- CTL hack
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped = ScriptUtils.mkUntypedValidator . mkValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Ledger.Script
serialisableValidator = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
