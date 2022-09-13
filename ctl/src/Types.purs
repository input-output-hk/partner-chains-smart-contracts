module Types where

import Contract.Prelude

import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol, TokenName, Value, valueOf)
import Contract.Value as Value
import Data.BigInt (BigInt)

type PubKey = ByteArray
type Signature = ByteArray -- Ed25519Signature

newtype AssetClass = AssetClass
  { currencySymbol ∷ CurrencySymbol, tokenName ∷ TokenName }

derive instance Generic AssetClass _
derive instance Newtype AssetClass _
instance ToData AssetClass where
  toData (AssetClass { currencySymbol, tokenName }) =
    Constr zero
      [ toData currencySymbol
      , toData tokenName
      ]

assetClass ∷ CurrencySymbol → TokenName → AssetClass
assetClass currencySymbol tokenName =
  AssetClass { currencySymbol, tokenName }

assetClassValueOf ∷ Value → AssetClass → BigInt
assetClassValueOf val (AssetClass { currencySymbol, tokenName }) =
  valueOf val currencySymbol tokenName

assetClassValue ∷ AssetClass → BigInt → Value
assetClassValue (AssetClass { currencySymbol, tokenName }) amount =
  Value.singleton currencySymbol tokenName amount
