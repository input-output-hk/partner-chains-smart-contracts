{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator (
  validator,
  hash,
  address,
) where

import Ledger qualified
import Ledger.Typed.Scripts (Validator)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)

{- | 'mkMptRootTokenValidator' always fails.

 TODO: There's a security issue here -- someone could steal the token so no
 one else has access to it (and the honest individual would have to inspect
 the blockchain, and mint such token themselves). There was an attempt at one
 point to make this "forward" the token
-}
{-# INLINEABLE mkMptRootTokenValidator #-}
mkMptRootTokenValidator :: SidechainParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMptRootTokenValidator _sc _dat _red _ctx = ()

-- This should be the following when we get reference inputs
-- > mkMptRootTokenValidator :: SidechainParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- > mkMptRootTokenValidator _sc _dat _red _ctx = Builtins.error ()

validator :: SidechainParams -> Validator
validator sc =
  Scripts.mkValidatorScript
    ( $$(PlutusTx.compile [||mkMptRootTokenValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
    )

hash :: SidechainParams -> ValidatorHash
hash = Scripts.validatorHash . Scripts.unsafeMkTypedValidator . validator

address :: SidechainParams -> Address
address = Ledger.scriptHashAddress . hash
