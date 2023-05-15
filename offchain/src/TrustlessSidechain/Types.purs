-- | `Types` includes some uility types (and functions); and types of certain
-- | endpoints.
module TrustlessSidechain.Types
  ( PubKey
  , Signature
  , Ed25519Signature
  , AssetClass
  , assetClass
  , assetClassValueOf
  , assetClassValue
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol, TokenName, Value, valueOf)
import Contract.Value as Value
import Data.BigInt (BigInt)

-- * Utility types relating to cryptography
type PubKey = ByteArray

type Signature = Ed25519Signature

type Ed25519Signature = ByteArray

-- * Utility types and functions for working with `CurrencySymbol`s and `TokenName`s
type AssetClass = CurrencySymbol /\ TokenName

assetClass ∷ CurrencySymbol → TokenName → AssetClass
assetClass currencySymbol tokenName =
  currencySymbol /\ tokenName

assetClassValueOf ∷ Value → AssetClass → BigInt
assetClassValueOf val (currencySymbol /\ tokenName) =
  valueOf val currencySymbol tokenName

assetClassValue ∷ AssetClass → BigInt → Value
assetClassValue (currencySymbol /\ tokenName) amount =
  Value.singleton currencySymbol tokenName amount
