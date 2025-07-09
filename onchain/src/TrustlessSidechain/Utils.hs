{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
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

import TrustlessSidechain.PlutusPrelude

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

-- | Unwrap a singleton list, or produce an error if not possible.
{-# INLINEABLE fromSingleton #-}
fromSingleton :: BuiltinString -> [a] -> a
fromSingleton _ [x] = x
fromSingleton msg _ = traceError msg

-- | Unwrap a singleton list, or produce an error if not possible.
{-# INLINEABLE fromSingletonData #-}
fromSingletonData :: (UnsafeFromData a) => BuiltinString -> List.List a -> a
fromSingletonData msg list = case List.uncons list of
  Just (x, rest) | List.null rest -> x
  _ -> traceError msg

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
oneTokenMinted :: DataV2.TxInfo -> DataV1.CurrencySymbol -> DataV1.TokenName -> Bool
oneTokenMinted txInfo cs tn =
  DataV1.valueOf (DataV2.txInfoMint txInfo) cs tn == 1

-- | Check that exactly one specified asset was burned by a transaction.  Note
-- that transaction is also allowed to burn tokens of the same 'CurrencySymbol',
-- but with different 'TokenName's.
{-# INLINEABLE oneTokenBurned #-}
oneTokenBurned :: Value -> CurrencySymbol -> TokenName -> Bool
oneTokenBurned txInfoMint cs tn =
  valueOf txInfoMint cs tn == -1

scriptToPlutusScript :: SerialisedScript -> PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2

{-# INLINEABLE getOutputsAt #-}
getOutputsAt :: DataV2.TxInfo -> DataV2.Address -> List.List DataV2.TxOut
getOutputsAt txInfo address =
  ((== address) . DataV2.txOutAddress) `List.filter` DataV2.txInfoOutputs txInfo

{-# INLINEABLE getInputsAt #-}
getInputsAt :: DataV2.TxInfo -> DataV2.Address -> List.List DataV2.TxOut
getInputsAt txInfo address =
  DataV2.txInInfoResolved
    `List.map` List.filter ((== address) . DataV2.txOutAddress . DataV2.txInInfoResolved) (DataV2.txInfoInputs txInfo)
