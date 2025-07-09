{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
  fromSingleton,
  fromJust,
  currencySymbolValueOf,
  oneTokenBurned,
  oneTokenMinted,
  scriptToPlutusScript,
  oneTokenMintedUnsafe,
  oneTokenBurnedUnsafe,
) where

import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
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

scriptToPlutusScript :: SerialisedScript -> PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2
