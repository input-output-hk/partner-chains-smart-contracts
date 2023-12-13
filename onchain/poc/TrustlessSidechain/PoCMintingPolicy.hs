{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.PoCMintingPolicy (
  serialisablePoCMintingPolicy,
) where

import Plutus.V2.Ledger.Api (Script, fromCompiledCode)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  txInfoMint,
 )
import PlutusTx (compile)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.ScriptUtils (mkUntypedMintingPolicy)
import TrustlessSidechain.Types (SidechainParams)
import TrustlessSidechain.Utils (currencySymbolValueOf)

{- | Dummy FUEL minting policy for testing purposes.  Allows minting a single
   dummy token.

 Note: this policy does not allow to burn transaction tokens, but we might
 wish to do so in order to allow recovering minAda.
-}
mkPoCMintingPolicy ::
  SidechainParams ->
  () -> -- no redeemer
  ScriptContext ->
  Bool
mkPoCMintingPolicy _ _ (ScriptContext txInfo (Minting currSymbol)) =
  traceIfFalse "ERROR-DUMMY-MINTING-01" (ttMinted > 0)
  where
    ttMinted = currencySymbolValueOf (txInfoMint txInfo) currSymbol
mkPoCMintingPolicy _ _ _ =
  trace "ERROR-DUMMY-MINTING-02" False

{-# INLINEABLE mkPoCMintingPolicyUntyped #-}
mkPoCMintingPolicyUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkPoCMintingPolicyUntyped params =
  mkUntypedMintingPolicy $ mkPoCMintingPolicy (unsafeFromBuiltinData params)

serialisablePoCMintingPolicy :: Script
serialisablePoCMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkPoCMintingPolicyUntyped||])
