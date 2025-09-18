{-# OPTIONS_GHC -fno-specialise #-}

{- |
Module      : PartnerChains.Utils
Description : Utility functions for Plutus scripts and test logic

This module provides utility functions for working with Plutus data structures.
-}
module PartnerChains.Utils (
  fromSingleton,
  fromSingletonData,
  fromJust,
  currencySymbolValueOf,
  oneTokenBurned,
  scriptToPlutusScript,
  oneTokenMinted,
  getOutputsAt,
  getInputsAt,
) where

import PlutusTx.Foldable (sum)
import PlutusTx.Prelude

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Data.V1 qualified as DataV1
import PlutusLedgerApi.Data.V2 qualified as DataV2
import PlutusLedgerApi.V1.Data.Value qualified as DataV1 (valueOf)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  TokenName,
  Value,
  getValue,
 )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Data.List qualified as List

-- | Unwrap a singleton list, or fail with a custom error.
fromSingleton ::
  -- | Error thunk evaluated if the list is not a singleton.
  -- We do this so the Plutus optimizer can strip out the error string when `remove-trace` is enabled.
  -- If the string is passed directly it will be included in the CBOR output even if not used.
  (() -> a) ->
  -- | Input list
  [a] ->
  -- | The only element in the list (or error)
  a
{-# INLINEABLE fromSingleton #-}
fromSingleton _ [x] = x
fromSingleton msg _ = msg ()

-- | Unwrap a singleton Plutus list, or fail with a custom error.
fromSingletonData ::
  (UnsafeFromData a) =>
  -- | Error thunk evaluated if the list is not a singleton.
  -- We do this so the Plutus optimizer can strip out the error string when `remove-trace` is enabled.
  -- If the string is passed directly it will be included in the CBOR output even if not used.
  (() -> a) ->
  -- | Plutus list
  List.List a ->
  -- | The only element in the list (or error)
  a
{-# INLINEABLE fromSingletonData #-}
fromSingletonData msg list = case List.uncons list of
  Just (x, rest) | List.null rest -> x
  _ -> msg ()

-- | Unwrap a 'Maybe' value, or fail with a custom error.
fromJust ::
  -- | Error thunk evaluated if the value is 'Nothing'
  -- We do this so the Plutus optimizer can strip out the error string when `remove-trace` is enabled.
  -- If the string is passed directly it will be included in the CBOR output even if not used.
  (() -> a) ->
  -- | Input optional value
  Maybe a ->
  -- | The contained value (or error)
  a
{-# INLINEABLE fromJust #-}
fromJust err m =
  case m of
    Just d -> d
    Nothing -> err ()

-- | Get the total amount of a given currency symbol in a value, ignoring token names.
currencySymbolValueOf ::
  -- | Value to inspect
  Value ->
  -- | Currency symbol to extract total quantity for
  CurrencySymbol ->
  -- | Total quantity of that currency symbol
  Integer
{-# INLINEABLE currencySymbolValueOf #-}
currencySymbolValueOf v c = maybe 0 sum $ Map.lookup c $ getValue v

-- | Check that exactly one token of the specified currency and token name was minted.
oneTokenMinted ::
  -- | Transaction info
  DataV2.TxInfo ->
  -- | Currency symbol
  DataV1.CurrencySymbol ->
  -- | Token name
  DataV1.TokenName ->
  -- | True if exactly one token was minted
  Bool
{-# INLINEABLE oneTokenMinted #-}
oneTokenMinted txInfo cs tn =
  DataV1.valueOf (DataV2.txInfoMint txInfo) cs tn == 1

-- | Check that exactly one token of the specified currency and token name was burned.
oneTokenBurned ::
  -- | The 'txInfoMint' field of the transaction, treated as a 'Value'
  Value ->
  -- | Currency symbol
  CurrencySymbol ->
  -- | Token name
  TokenName ->
  -- | True if exactly one token was burned
  Bool
{-# INLINEABLE oneTokenBurned #-}
oneTokenBurned txInfoMint cs tn =
  valueOf txInfoMint cs tn == -1

-- | Convert a serialized Plutus script to a 'PlutusScript' suitable for submission via the Cardano API.
scriptToPlutusScript ::
  -- | Serialized script (from 'PlutusTx.compile')
  SerialisedScript ->
  -- | Wrapped script for use in transactions
  PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2

-- | Get all outputs that pay to the specified address.
getOutputsAt ::
  -- | Transaction info
  DataV2.TxInfo ->
  -- | Address to filter for
  DataV2.Address ->
  -- | Outputs paying to that address
  List.List DataV2.TxOut
{-# INLINEABLE getOutputsAt #-}
getOutputsAt txInfo address =
  ((== address) . DataV2.txOutAddress) `List.filter` DataV2.txInfoOutputs txInfo

-- | Get all resolved inputs that come from the specified address.
getInputsAt ::
  -- | Transaction info
  DataV2.TxInfo ->
  -- | Address to filter for
  DataV2.Address ->
  -- | Inputs that were spent from that address
  List.List DataV2.TxOut
{-# INLINEABLE getInputsAt #-}
getInputsAt txInfo address =
  DataV2.txInInfoResolved
    `List.map` List.filter ((== address) . DataV2.txOutAddress . DataV2.txInInfoResolved) (DataV2.txInfoInputs txInfo)
