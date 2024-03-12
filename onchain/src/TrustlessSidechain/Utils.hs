{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
  fromSingleton,
  currencySymbolValueOf,
  oneTokenBurned,
  oneTokenMinted,
  mkUntypedValidator,
  mkUntypedMintingPolicy,
  scriptToPlutusScript,
  oneTokenMintedRaw,
  oneTokenBurnedRaw,
) where

import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Kind (Type)
import Plutonomy.UPLC qualified
import Plutus.V1.Ledger.Scripts (Script)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (
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

{-# INLINEABLE oneTokenMintedRaw #-}
oneTokenMintedRaw :: Unsafe.TxInfo -> CurrencySymbol -> TokenName -> Bool
oneTokenMintedRaw txInfoRaw cs tn =
  valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfoRaw) cs tn == 1

-- | Check that exactly one specified asset was burned by a transaction.  Note
-- that transaction is also allowed to burn tokens of the same 'CurrencySymbol',
-- but with different 'TokenName's.  This is intended for use with 'InitToken's,
-- so that we permit multiple 'InitToken's with different names burned in the
-- same transaction.
{-# INLINEABLE oneTokenBurned #-}
oneTokenBurned :: Value -> CurrencySymbol -> TokenName -> Bool
oneTokenBurned txInfoMint cs tn =
  valueOf txInfoMint cs tn == -1

{-# INLINEABLE oneTokenBurnedRaw #-}
oneTokenBurnedRaw :: Unsafe.TxInfo -> CurrencySymbol -> TokenName -> Bool
oneTokenBurnedRaw txInfoRaw cs tn =
  valueOf (Unsafe.decode $ Unsafe.txInfoMint txInfoRaw) cs tn == -1

-- | Convert a validator to untyped
-- The output will accept BuiltinData instead of concrete types
{-# INLINE mkUntypedValidator #-}
mkUntypedValidator ::
  forall (d :: Type) (r :: Type).
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
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
  (BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if
-- parsing failed
mkUntypedMintingPolicy f r p =
  check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

scriptToPlutusScript :: Script -> PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2
    . toShort
    . toStrict
    . serialise
    . Plutonomy.UPLC.optimizeUPLC
