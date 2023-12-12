{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
  fromSingleton,
  currencySymbolValueOf,
) where

import Plutus.V1.Ledger.Value (CurrencySymbol, Value, getValue)
import PlutusTx.AssocMap qualified as Map
import TrustlessSidechain.PlutusPrelude

-- | Unwrap a singleton list, or produce an error if not possible.
{-# INLINEABLE fromSingleton #-}
fromSingleton :: BuiltinString -> [a] -> a
fromSingleton _ [x] = x
fromSingleton msg _ = traceError msg

-- | Get amount of given currency in a value, ignoring token names.
{-# INLINEABLE currencySymbolValueOf #-}
currencySymbolValueOf :: Value -> CurrencySymbol -> Integer
currencySymbolValueOf v c = case Map.lookup c (getValue v) of
  Nothing -> 0
  Just x -> sum (Map.elems x)
