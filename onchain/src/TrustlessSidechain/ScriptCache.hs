{-# LANGUAGE TemplateHaskell #-}

{- | 'TrustlessSidechain.ScriptCache' module provides a simple on-chain script
 for storing UTxOs with attached reference scripts.  It acts as a cache for
 minting policies of tokens minted during sidechain initialization.
-}
module TrustlessSidechain.ScriptCache (serialisableScriptCache) where

import Ledger (Language (PlutusV2), PubKeyHash, Script, ScriptContext (ScriptContext), Versioned (Versioned), fromCompiledCode, txSignedBy)
import PlutusTx (compile, unsafeFromBuiltinData)
import TrustlessSidechain.PlutusPrelude

{- | Script cache parameterized by a public key hash.  Spending from the script
 is only permitted when the transaction is signed by pub key hash used as the
 script parameter.
-}
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
mkScriptCacheUntyped pkh datum redeemer sc =
  check $
    mkScriptCache
      (PlutusTx.unsafeFromBuiltinData pkh)
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (PlutusTx.unsafeFromBuiltinData sc)

serialisableScriptCache :: Versioned Script
serialisableScriptCache =
  Versioned
    (fromCompiledCode $$(PlutusTx.compile [||mkScriptCacheUntyped||]))
    PlutusV2
