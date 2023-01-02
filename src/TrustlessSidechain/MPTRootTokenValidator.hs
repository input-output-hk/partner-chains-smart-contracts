{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.MPTRootTokenValidator (
  serialisableValidator,
) where

import PlutusTx.Prelude

-- dangerously contains lots of v1 stuff

import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import PlutusTx (compile, unsafeFromBuiltinData)
import PlutusTx.Trace qualified as Trace
import TrustlessSidechain.Types (SidechainParams)

-- | 'mkMptRootTokenValidator' always fails.
{-# INLINEABLE mkMptRootTokenValidator #-}
mkMptRootTokenValidator :: SidechainParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMptRootTokenValidator _sc _dat _red _ctx =
  Trace.traceError "error 'mkMptRootTokenValidator': illegal attempt to spend"

-- CTL hack
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped = mkMptRootTokenValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Versioned Ledger.Script
serialisableValidator =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])) PlutusV2
