{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.MerkleRootTokenValidator (
  serialisableValidator,
) where

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import PlutusTx (compile, unsafeFromBuiltinData)
import PlutusTx.Trace qualified as Trace
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (SidechainParams)

-- | 'mkMptRootTokenValidator' always fails.
--
-- OnChain error descriptions:
--
--   ERROR-MERKLE-ROOT-TOKEN-VALIDATOR-01: Illegal attempt to spend.
{-# INLINEABLE mkMptRootTokenValidator #-}
mkMptRootTokenValidator ::
  SidechainParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkMptRootTokenValidator _sc _dat _red _ctx =
  Trace.traceError "ERROR-MERKLE-ROOT-TOKEN-VALIDATOR-01"

mkValidatorUntyped ::
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped = mkMptRootTokenValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Script
serialisableValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
