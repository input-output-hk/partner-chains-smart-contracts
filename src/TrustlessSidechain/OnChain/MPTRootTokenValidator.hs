{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator where

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)

{-# INLINEABLE mkValidator #-}
mkValidator :: SidechainParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ _ = ()

-- When we have reference inputs, we should replace the above line with
-- > mkValidator _ _ _ _ = Builtins.error ()

-- If it's of any interest, the following was the old typed version...
-- > typedValidator :: SidechainParams -> Script.TypedValidator MPT
-- > typedValidator p =
-- >   Script.mkTypedValidator @MPT
-- >     ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
-- >     $$(PlutusTx.compile [||wrap||])
-- >   where
-- >     wrap = Script.wrapValidator @() @()

validator :: SidechainParams -> Ledger.Validator
validator sc =
  Scripts.mkValidatorScript
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
    )

hash :: SidechainParams -> Ledger.ValidatorHash
hash = Scripts.validatorHash . Scripts.unsafeMkTypedValidator . validator

address :: SidechainParams -> Ledger.Address
address = Ledger.scriptHashAddress . hash
