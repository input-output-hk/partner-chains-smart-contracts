{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
  fromSingleton,
  fromJust,
  currencySymbolValueOf,
  oneTokenBurned,
  oneTokenMinted,
  mkUntypedValidator,
  mkUntypedMintingPolicy,
  scriptToPlutusScript,
  oneTokenMintedUnsafe,
  oneTokenBurnedUnsafe,
) where

import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Data.Kind (Type)
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  ScriptContext,
  TokenName,
  Value,
  getValue,
 )
import PlutusTx.AssocMap qualified as Map

-- | Unwrap a singleton list, or produce an error if not possible.
{-# INLINEABLE fromSingleton #-}
fromSingleton :: BuiltinString -> [a] -> a
fromSingleton _ [x] = x
fromSingleton msg _ = traceError msg

-- | Unwrap a Just ctor, or produce an error if not possible.
{-# INLINEABLE fromJust #-}
fromJust :: forall a. BuiltinString -> Maybe a -> a
fromJust err m =
  case m of
    Just d -> d
    Nothing -> traceError err

-- | Get amount of given currency in a value, ignoring token names.
{-# INLINEABLE currencySymbolValueOf #-}
currencySymbolValueOf :: Value -> CurrencySymbol -> Integer
currencySymbolValueOf v c = maybe 0 sum $ Map.lookup c $ getValue v

-- | Check that exactly on specified asset was minted by a transaction.  Note
-- that transaction is also allowed to mint/burn tokens of the same
-- 'CurrencySymbol', but with different 'TokenName's.
{-# INLINEABLE oneTokenMinted #-}
oneTokenMinted :: Value -> CurrencySymbol -> TokenName -> Bool
oneTokenMinted txInfoMint cs tn =
  valueOf txInfoMint cs tn == 1

{-# INLINEABLE oneTokenMintedUnsafe #-}
oneTokenMintedUnsafe :: Unsafe.TxInfo -> CurrencySymbol -> TokenName -> Bool
oneTokenMintedUnsafe txInfo cs tn =
  valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) cs tn == 1

-- | Check that exactly one specified asset was burned by a transaction.  Note
-- that transaction is also allowed to burn tokens of the same 'CurrencySymbol',
-- but with different 'TokenName's.
{-# INLINEABLE oneTokenBurned #-}
oneTokenBurned :: Value -> CurrencySymbol -> TokenName -> Bool
oneTokenBurned txInfoMint cs tn =
  valueOf txInfoMint cs tn == -1

{-# INLINEABLE oneTokenBurnedUnsafe #-}
oneTokenBurnedUnsafe :: Unsafe.TxInfo -> CurrencySymbol -> TokenName -> Bool
oneTokenBurnedUnsafe txInfo cs tn =
  valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo) cs tn == -1

-- | Convert a validator to untyped
-- The output will accept BuiltinData instead of concrete types
{-# INLINE mkUntypedValidator #-}
mkUntypedValidator ::
  forall (d :: Type) (r :: Type).
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if
-- parsing failed
mkUntypedValidator f d r p =
  check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

-- | Convert a minting policy to untyped
-- The output will accept BuiltinData instead of concrete types
{-# INLINE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy ::
  forall (r :: Type).
  (UnsafeFromData r) =>
  (r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinUnit)
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if
-- parsing failed
mkUntypedMintingPolicy f r p =
  check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

scriptToPlutusScript :: SerialisedScript -> PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2
