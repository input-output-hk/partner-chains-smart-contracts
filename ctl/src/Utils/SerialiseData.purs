-- | This module includes functionality relating to serializing
-- | `Contract.PlutusData` via cbor that is compatible with the onchain
-- | Builtin serialiseData.
module Utils.SerialiseData (serialiseData, serialiseToData) where

import Contract.Prelude

import Contract.PlutusData (class ToData, PlutusData)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Serialization as Serialization
import Serialization.PlutusData as SerializationPlutusData
import Untagged.Union as Union

-- | `serialiseData` is the offchain version of the Builtin `serialiseData`.
serialiseData ∷ PlutusData → Maybe ByteArray
serialiseData = ((Serialization.toBytes <<< Union.asOneOf) <$> _) <<<
  SerializationPlutusData.convertPlutusData

-- | `serialiseToData` is a convenient wrapper around:
-- |
-- |    1. convert the given type into `PlutusData`
-- |
-- |    2. running `Utils.SerialiseData.serialiseData`
serialiseToData ∷ ∀ a. ToData a ⇒ a → Maybe ByteArray
serialiseToData = serialiseData <<< PlutusData.toData
