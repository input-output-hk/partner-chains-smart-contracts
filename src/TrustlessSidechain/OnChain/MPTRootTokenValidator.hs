{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator (
  validator,
  hash,
  address,
  serialisableValidator,
) where

import PlutusTx.Prelude

import Ledger qualified -- dangerously contains lots of v1 stuff
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Scripts (Validator)
import Plutus.V2.Ledger.Api (
  Address,
  ValidatorHash,
  mkValidatorScript,
 )
import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData)
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
  mkValidatorScript
    ( $$(PlutusTx.compile [||mkMptRootTokenValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
    )

hash :: SidechainParams -> ValidatorHash
hash = Scripts.validatorHash . Scripts.unsafeMkTypedValidator . validator

address :: SidechainParams -> Address
address = Ledger.scriptHashAddress . hash

-- CTL hack
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped = mkMptRootTokenValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Ledger.Script
serialisableValidator = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
