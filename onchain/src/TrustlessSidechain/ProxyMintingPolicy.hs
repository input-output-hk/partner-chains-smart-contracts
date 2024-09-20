{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.ProxyMintingPolicy (
  serialisableProxyMintingPolicy,
) where

import PlutusLedgerApi.V2 (
  SerialisedScript,
  serialiseCompiledCode,
 )
import qualified PlutusTx
import TrustlessSidechain.PlutusPrelude (
  Bool,
  BuiltinData,
  Integer,
  check,
  fromInteger,
  fromString,
  negate,
  traceIfFalse,
  ($),
  (&&),
  (<),
  (==),
  (>),
 )
import TrustlessSidechain.Types (ProxyMintingPolicyRedeemer (BurnProxyToken, MintProxyToken))
import qualified TrustlessSidechain.Types.Unsafe as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  getVersionedCurrencySymbolUnsafe,
 )

{-# INLINEABLE mkProxyMintingPolicy #-}
mkProxyMintingPolicy :: VersionOracleConfig -> Integer -> Integer -> ProxyMintingPolicyRedeemer -> Unsafe.ScriptContext -> Bool
mkProxyMintingPolicy versioningConfig mintScriptId _burnScriptId (MintProxyToken version) sc =
  traceIfFalse "ERROR-FUEL-POLICY-01" sameAmountsMinted
    && traceIfFalse "ERROR-FUEL-POLICY-02" tokensMinted
  where
    txInfo = Unsafe.scriptContextTxInfo sc
    currSymbol = Unsafe.ownCurrencySymbol sc
    -- Get currency symbol of a versioned sub minting policy.
    coreCurrSymbol =
      getVersionedCurrencySymbolUnsafe
        versioningConfig
        (VersionOracle {version, scriptId = mintScriptId})
        sc

    mintValue = Unsafe.decode (Unsafe.txInfoMint txInfo)
    -- Amount of minted tokens.
    mintedAmount = currencySymbolValueOf mintValue currSymbol

    -- Ensure that proxy tokens are minted and not burned.
    tokensMinted = mintedAmount > 0

    -- Check that amount of proxy minted in this transaction is the same
    -- as amount of tokens minted by versioned minting policy.
    sameAmountsMinted =
      currencySymbolValueOf mintValue coreCurrSymbol == mintedAmount
mkProxyMintingPolicy versioningConfig _mintScriptId burnScriptId (BurnProxyToken version) sc =
  traceIfFalse "ERROR-FUEL-POLICY-03" burnedAmountEqualToMintedAmount
    && traceIfFalse "ERROR-FUEL-POLICY-04" tokensBurned
  where
    txInfo = Unsafe.scriptContextTxInfo sc
    currSymbol = Unsafe.ownCurrencySymbol sc
    -- Get currency symbol of a versioned sub minting policy.
    coreCurrSymbol =
      getVersionedCurrencySymbolUnsafe
        versioningConfig
        (VersionOracle {version, scriptId = burnScriptId})
        sc

    mintValue = Unsafe.decode (Unsafe.txInfoMint txInfo)
    -- Amount of burned tokens.
    burnedAmount = currencySymbolValueOf mintValue currSymbol

    -- Ensure that proxy tokens are burned and not minted.
    tokensBurned = burnedAmount < 0

    -- Check that amount of proxy burned in this transaction is the same
    -- as amount of tokens minted by versioned minting policy.
    burnedAmountEqualToMintedAmount =
      currencySymbolValueOf mintValue coreCurrSymbol == (negate burnedAmount)

{-# INLINEABLE mkProxyMintingPolicyUntyped #-}
mkProxyMintingPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkProxyMintingPolicyUntyped versionOracleConfig mint burn redeemer ctx =
  check $
    mkProxyMintingPolicy
      (PlutusTx.unsafeFromBuiltinData versionOracleConfig)
      (PlutusTx.unsafeFromBuiltinData mint)
      (PlutusTx.unsafeFromBuiltinData burn)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableProxyMintingPolicy :: SerialisedScript
serialisableProxyMintingPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkProxyMintingPolicyUntyped||])
