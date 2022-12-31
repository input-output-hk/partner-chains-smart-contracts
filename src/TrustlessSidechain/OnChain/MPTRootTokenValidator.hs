{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.MPTRootTokenValidator (
  validator,
  hash,
  address,
  serialisableValidator,
) where

import PlutusTx.Prelude

-- dangerously contains lots of v1 stuff

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Scripts (Validator)
import Plutus.V2.Ledger.Api (
  Address,
  ValidatorHash,
  mkValidatorScript,
 )
import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData)
import PlutusTx.Trace qualified as Trace
import TrustlessSidechain.OnChain.Types (SidechainParams)

-- | 'mkMptRootTokenValidator' always fails.
{-# INLINEABLE mkMptRootTokenValidator #-}
mkMptRootTokenValidator :: SidechainParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMptRootTokenValidator _sc _dat _red _ctx = Trace.traceError "error 'mkMptRootTokenValidator': illegal attempt to spend"

validator :: SidechainParams -> Validator
validator sc =
  mkValidatorScript
    ( $$(PlutusTx.compile [||mkMptRootTokenValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
    )

hash :: SidechainParams -> ValidatorHash
hash scParams =
  Scripts.validatorHash
    . Scripts.unsafeMkTypedValidator
    $ Ledger.Versioned (validator scParams) Ledger.PlutusV2

address :: SidechainParams -> Address
address = Ledger.scriptHashAddress . hash

-- CTL hack
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped = mkMptRootTokenValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Versioned Ledger.Script
serialisableValidator = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])) PlutusV2
