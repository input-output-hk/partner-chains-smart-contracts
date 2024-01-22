{-# LANGUAGE TemplateHaskell #-}

-- | 'TrustlessSidechain.GenesisUtxoCache' module provides a simple on-chain
-- script for storing the genesis UTxO so that it doesn't get spent before
-- initialization.
module TrustlessSidechain.GenesisUtxoCache (
  serialisableGenesisUtxoCache,
) where

import Plutus.V2.Ledger.Api (PubKeyHash, Script, fromCompiledCode)
import Plutus.V2.Ledger.Contexts (ScriptContext (ScriptContext), txSignedBy)
import PlutusTx (compile, unsafeFromBuiltinData)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  mkUntypedValidator,
 )

-- | Cache script for safely storing the genesis utxo so that it doesn't get
-- spent before sidechain initialization.  The only way to prevent that is to
-- store this UTxO outside the wallet address.
--
-- OnChain error descriptions:
--
--   ERROR-GENESIS-UTXO-OWNER-01: Not signed by owner.
{-# INLINEABLE mkGenesisUtxoCache #-}
mkGenesisUtxoCache ::
  -- | Chain ID
  Integer ->
  -- | UTxO owner datum
  PubKeyHash ->
  () ->
  ScriptContext ->
  Bool
mkGenesisUtxoCache _ pkh () (ScriptContext txInfo _) =
  traceIfFalse "ERROR-GENESIS-UTXO-OWNER-01" isOwner
  where
    isOwner = txSignedBy txInfo pkh

{-# INLINEABLE mkGenesisUtxoCacheUntyped #-}
mkGenesisUtxoCacheUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkGenesisUtxoCacheUntyped pkh =
  mkUntypedValidator $
    mkGenesisUtxoCache
      (PlutusTx.unsafeFromBuiltinData pkh)

serialisableGenesisUtxoCache :: Script
serialisableGenesisUtxoCache =
  fromCompiledCode $$(PlutusTx.compile [||mkGenesisUtxoCacheUntyped||])
