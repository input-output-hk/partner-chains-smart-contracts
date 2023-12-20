{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
  fromSingleton,
  currencySymbolValueOf,
  mkUntypedValidator,
  mkUntypedMintingPolicy,
  scriptToPlutusScript,
) where

import TrustlessSidechain.PlutusPrelude

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Kind (Type)
import Plutonomy.UPLC qualified
import Plutus.V1.Ledger.Scripts (Script)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, getValue)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.AssocMap qualified as Map

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

{- | Convert a validator to untyped
 The output will accept BuiltinData instead of concrete types
-}
mkUntypedValidator ::
  forall (d :: Type) (r :: Type).
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if
-- parsing failed
mkUntypedValidator f d r p =
  check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

{- | Convert a minting policy to untyped
 The output will accept BuiltinData instead of concrete types
-}
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
