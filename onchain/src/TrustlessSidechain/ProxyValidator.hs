{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.ProxyValidator (
  serialisableProxyValidator,
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
  traceIfFalse,
  ($),
  (>),
 )
import qualified TrustlessSidechain.Types.Unsafe as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle, scriptId, version),
  VersionOracleConfig,
  getVersionedCurrencySymbolUnsafe,
 )

{-# INLINEABLE mkProxyValidator #-}
mkProxyValidator :: VersionOracleConfig -> Integer -> BuiltinData -> Integer -> Unsafe.ScriptContext -> Bool
mkProxyValidator versioningConfig mintScriptId _datum version sc =
  traceIfFalse "ERROR-FUEL-POLICY-01" tokensMinted
  where
    txInfo = Unsafe.scriptContextTxInfo sc
    -- Get currency symbol of a versioned sub minting policy.
    coreCurrSymbol =
      getVersionedCurrencySymbolUnsafe
        versioningConfig
        (VersionOracle {version, scriptId = mintScriptId})
        sc

    mintValue = Unsafe.decode (Unsafe.txInfoMint txInfo)

    -- Amount of minted tokens.
    mintedAmount = currencySymbolValueOf mintValue coreCurrSymbol

    -- Ensure that sub minting policy tokens are minted.
    tokensMinted = mintedAmount > 0

{-# INLINEABLE mkProxyValidatorUntyped #-}
mkProxyValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkProxyValidatorUntyped versionOracleConfig mint datum redeemer ctx =
  check $
    mkProxyValidator
      (PlutusTx.unsafeFromBuiltinData versionOracleConfig)
      (PlutusTx.unsafeFromBuiltinData mint)
      datum
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (Unsafe.wrap ctx)

serialisableProxyValidator :: SerialisedScript
serialisableProxyValidator =
  serialiseCompiledCode $$(PlutusTx.compile [||mkProxyValidatorUntyped||])
