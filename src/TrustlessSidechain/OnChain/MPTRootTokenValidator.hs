{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator where

import Ledger (
  ScriptContext,
 )
import Ledger qualified
import Ledger.Typed.Scripts qualified as Script
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)

data MPT
instance Script.ValidatorTypes MPT where
  type RedeemerType MPT = ()
  type DatumType MPT = ()

{-# INLINEABLE mkValidator #-}
mkValidator :: SidechainParams -> () -> () -> ScriptContext -> Bool
mkValidator _ () () _ = True

--
-- When we have reference inputs, we should replace the above line with
-- > mkValidator _ () () _ = False

typedValidator :: SidechainParams -> Script.TypedValidator MPT
typedValidator p =
  Script.mkTypedValidator @MPT
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.wrapValidator @() @()

validator :: SidechainParams -> Ledger.Validator
validator = Script.validatorScript . typedValidator

hash :: SidechainParams -> Ledger.ValidatorHash
hash = Script.validatorHash . typedValidator

address :: SidechainParams -> Ledger.Address
address = Ledger.scriptHashAddress . hash
