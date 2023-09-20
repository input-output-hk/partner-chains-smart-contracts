{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.DummyMintingPolicy (
  serialisableDummyMintingPolicy,
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
mkDummyMintingPolicy ::
  SidechainParams ->
  () -> -- no redeemer
  ScriptContext ->
  Bool
mkDummyMintingPolicy _ _ (ScriptContext txInfo (Minting currSymbol)) =
  traceIfFalse "ERROR-DUMMY-MINTING-01" (ttMinted > 0)
  where
    ttMinted = currencySymbolValueOf (txInfoMint txInfo) currSymbol
mkDummyMintingPolicy _ _ _ =
  trace "ERROR-DUMMY-MINTING-02" False

{-# INLINEABLE mkDummyMintingPolicyUntyped #-}
mkDummyMintingPolicyUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkDummyMintingPolicyUntyped params =
  mkUntypedMintingPolicy $ mkDummyMintingPolicy (unsafeFromBuiltinData params)

serialisableDummyMintingPolicy :: Script
serialisableDummyMintingPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkDummyMintingPolicyUntyped||])
