{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator where

import Ledger qualified
import Plutus.Script.Utils.V2.Scripts qualified as Script
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (PassiveBrdgSidechainParams)

{-# INLINEABLE mkValidator #-}
mkValidator :: PassiveBrdgSidechainParams -> () -> () -> ScriptContext -> Bool
mkValidator _ () () _ = False

validator :: PassiveBrdgSidechainParams -> Script.Validator
validator p =
  Ledger.mkValidatorScript
    ($$(compile [||untypedValidator||]) `applyCode` liftCode p)
  where
    untypedValidator = Script.mkUntypedValidator . mkValidator

hash :: PassiveBrdgSidechainParams -> Ledger.ValidatorHash
hash = Script.validatorHash . validator

address :: PassiveBrdgSidechainParams -> Ledger.Address
address = Ledger.scriptHashAddress . hash
