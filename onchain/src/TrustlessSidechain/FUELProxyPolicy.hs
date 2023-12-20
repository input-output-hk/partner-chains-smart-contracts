{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | 'TrustlessSidechain.FUELProxyPolicy' module provides a so called
 proxy FUEL policy.  This policy delegates actual fuel minting and burning
 criteria to scripts stored in the versioning system, allowing to change
 minting and burning conditions by changing the versioned scripts and at the
 same time having a FUEL currency symbol that remains stable throughout the
 sidechain lifetime.
-}
module TrustlessSidechain.FUELProxyPolicy (
  serialisableFuelProxyPolicy,
) where

import Plutus.V2.Ledger.Api
import PlutusTx
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types
import TrustlessSidechain.Utils (currencySymbolValueOf, mkUntypedMintingPolicy)
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  fuelBurningPolicyId,
  fuelMintingPolicyId,
  getVersionedCurrencySymbol,
 )

{- | Redeemer for the proxy FUEL that tells whether fuel should be minted or
 burned, and which version of the fuel script to use.  Burn case also contains
 address of the sidechain recipient.  Recipient information is used by the
 sidechain bridge to add tokens to respective sidechain account.
-}
data FuelProxyRedeemer
  = FuelProxyMint {version :: Integer}
  | FuelProxyBurn
      { version :: Integer
      , -- | Recipient's sidechain address
        recipient :: BuiltinByteString
      }

makeIsDataIndexed
  ''FuelProxyRedeemer
  [ ('FuelProxyMint, 0)
  , ('FuelProxyBurn, 1)
  ]

{- | Mint/burn proxy FUEL tokens.  Delegates actual minting/burning logic to
 versioned FUEL policies, allowing to mint/burn N tokens if the respective
 versioned policy mints N tokens.  Note that this policy does not constrain
 the token name in any way and only checks the currency symbol.
-}
mkFuelProxyPolicy ::
  SidechainParams ->
  VersionOracleConfig ->
  FuelProxyRedeemer ->
  ScriptContext ->
  Bool
mkFuelProxyPolicy
  _
  versioningConfig
  FuelProxyMint {..}
  sc@(ScriptContext txInfo (Minting currSymbol)) =
    traceIfFalse "ERROR-FUEL-POLICY-01" sameAmountsMinted
      && traceIfFalse "ERROR-FUEL-POLICY-02" tokensMinted
    where
      -- Get currency symbol of a versioned fuel minting policy.
      coreCurrSymbol =
        getVersionedCurrencySymbol
          versioningConfig
          (VersionOracle {version, scriptId = fuelMintingPolicyId})
          sc

      -- Amount of minted tokens.
      mintedAmount = currencySymbolValueOf (txInfoMint txInfo) currSymbol

      -- Ensure that proxy fuel tokens are minted and not burned.
      tokensMinted = mintedAmount > 0

      -- Check that amount of proxy fuel minted in this transaction is the same
      -- as amount of tokens minted by versioned fuel minting policy.
      sameAmountsMinted =
        currencySymbolValueOf (txInfoMint txInfo) coreCurrSymbol == mintedAmount
mkFuelProxyPolicy
  _
  versioningConfig
  FuelProxyBurn {..}
  sc@(ScriptContext txInfo (Minting currSymbol)) =
    traceIfFalse "ERROR-FUEL-POLICY-03" sameAmountsMinted
      && traceIfFalse "ERROR-FUEL-POLICY-04" tokensBurned
    where
      -- Get currency symbol of a versioned fuel burning policy.
      coreCurrSymbol =
        getVersionedCurrencySymbol
          versioningConfig
          (VersionOracle {version, scriptId = fuelBurningPolicyId})
          sc

      -- Amount of burned tokens.
      mintedAmount = currencySymbolValueOf (txInfoMint txInfo) currSymbol

      -- Ensure that proxy fuel tokens are burned and not minted.
      tokensBurned = mintedAmount < 0

      -- Check that amount of proxy fuel burned in this transaction is the same
      -- as amount of tokens minted by versioned fuel burning policy.
      sameAmountsMinted =
        currencySymbolValueOf (txInfoMint txInfo) coreCurrSymbol
          == negate mintedAmount
mkFuelProxyPolicy _ _ _ _ =
  trace "ERROR-FUEL-POLICY-05" False

{-# INLINEABLE mkFuelProxyPolicyUntyped #-}
mkFuelProxyPolicyUntyped ::
  -- | Sidechain parameters
  BuiltinData ->
  -- | Versioning config
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkFuelProxyPolicyUntyped params versioningConfig =
  mkUntypedMintingPolicy $
    mkFuelProxyPolicy
      (unsafeFromBuiltinData params)
      (unsafeFromBuiltinData versioningConfig)

serialisableFuelProxyPolicy :: Script
serialisableFuelProxyPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkFuelProxyPolicyUntyped||])
