module Types where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Value as Value

type PubKey = ByteArray
type AssetClass = Value.CurrencySymbol /\ Value.TokenName
type Signature = ByteArray -- Ed25519Signature
