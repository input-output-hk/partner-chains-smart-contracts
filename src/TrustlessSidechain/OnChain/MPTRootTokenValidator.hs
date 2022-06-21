{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator where

import Ledger qualified
import Plutus.Script.Utils.V2.Scripts qualified as Script
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)

{-# INLINEABLE mkValidator #-}
mkValidator :: SidechainParams -> () -> () -> ScriptContext -> Bool
mkValidator _ () () _ = False

validator :: SidechainParams -> Script.Validator
validator p =
  Ledger.mkValidatorScript
    ($$(compile [||untypedValidator||]) `applyCode` liftCode p)
  where
    untypedValidator = Script.mkUntypedValidator . mkValidator

hash :: SidechainParams -> Ledger.ValidatorHash
hash = Script.validatorHash . validator

address :: SidechainParams -> Ledger.Address
address = Ledger.scriptHashAddress . hash
