-- | `Types` includes some uility types (and functions); and types of certain
-- | endpoints.
module TrustlessSidechain.Types
  ( PubKey
  , Signature
  , Ed25519Signature
  , CurrencyInfo
  ) where

import Contract.Prelude

import Cardano.Types.PlutusScript (PlutusScript, hash)

import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Prim.ByteArray (ByteArray)
import Cardano.Types.AssetName (AssetName)
import Contract.Value (CurrencySymbol, Value, valueOf)
import Contract.Value as Value
import Data.BigInt (BigInt)

-- * Utility types relating to cryptography
type PubKey = ByteArray

type Signature = Ed25519Signature

type Ed25519Signature = ByteArray

-- | Commonly used currency information packed paired together
type CurrencyInfo =
  { mintingPolicy ∷ PlutusScript
  , currencySymbol ∷ ScriptHash
  }

