{-# LANGUAGE TemplateHaskell #-}

-- | 'TrustlessSidechain.ScriptCache' module provides a simple on-chain script
-- for storing UTxOs with attached reference scripts.  It acts as a cache for
-- minting policies of tokens minted during sidechain initialization.
module TrustlessSidechain.ScriptCache (
  serialisableScriptCache,
) where

import PlutusLedgerApi.V2 (PubKeyHash, SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2.Contexts (ScriptContext (ScriptContext), txSignedBy)
import PlutusTx (compile, unsafeFromBuiltinData)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils (
  mkUntypedValidator,
 )

-- | Script cache parameterized by a public key hash.  Spending from the script
-- is only permitted when the transaction is signed by pub key hash used as the
-- script parameter.
--
-- OnChain error descriptions:
--
--   ERROR-SCRIPT-CACHE-01: Not signed by owner.
{-# INLINEABLE mkScriptCache #-}
mkScriptCache :: PubKeyHash -> () -> () -> ScriptContext -> Bool
mkScriptCache pkh () () (ScriptContext txInfo _) =
  traceIfFalse "ERROR-SCRIPT-CACHE-01" isOwner
  where
    isOwner = txSignedBy txInfo pkh

{-# INLINEABLE mkScriptCacheUntyped #-}
mkScriptCacheUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkScriptCacheUntyped pkh =
  mkUntypedValidator $
    mkScriptCache
      (PlutusTx.unsafeFromBuiltinData pkh)

serialisableScriptCache :: SerialisedScript
serialisableScriptCache =
  serialiseCompiledCode $$(PlutusTx.compile [||mkScriptCacheUntyped||])
