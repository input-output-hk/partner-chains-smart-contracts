{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnlyMintMintingPolicy (
  serialisableOnlyMintMintingPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  txInfoMint,
 )
import PlutusTx (compile)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (SidechainParams)
import TrustlessSidechain.Utils (currencySymbolValueOf, mkUntypedMintingPolicy)

-- | Dummy FUEL minting policy for testing purposes.  Allows minting a single
--   dummy token.
--
-- Note: this policy does not allow to burn transaction tokens, but we might
-- wish to do so in order to allow recovering minAda.
mkOnlyMintMintingPolicy ::
  SidechainParams ->
  () -> -- no redeemer
  ScriptContext ->
  Bool
mkOnlyMintMintingPolicy _ _ (ScriptContext txInfo (Minting currSymbol)) =
  traceIfFalse "ERROR-DUMMY-MINTING-01" (ttMinted > 0)
  where
    ttMinted = currencySymbolValueOf (txInfoMint txInfo) currSymbol
mkOnlyMintMintingPolicy _ _ _ =
  trace "ERROR-DUMMY-MINTING-02" False

{-# INLINEABLE mkOnlyMintMintingPolicyUntyped #-}
mkOnlyMintMintingPolicyUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkOnlyMintMintingPolicyUntyped params =
  mkUntypedMintingPolicy $ mkOnlyMintMintingPolicy (unsafeFromBuiltinData params)

serialisableOnlyMintMintingPolicy :: SerialisedScript
serialisableOnlyMintMintingPolicy =
  serialiseCompiledCode
    $$(compile [||mkOnlyMintMintingPolicyUntyped||])
