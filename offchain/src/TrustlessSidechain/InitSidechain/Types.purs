module TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  , InitTokenRedeemer(MintInitToken, BurnInitToken)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer)
  )
import Contract.Value (CurrencySymbol, TokenName)
import JS.BigInt as BigInt
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productToData2
  )

data InitTokenRedeemer
  = MintInitToken
  | BurnInitToken

derive instance Eq InitTokenRedeemer

derive instance Generic InitTokenRedeemer _

instance Show InitTokenRedeemer where
  show = genericShow

instance ToData InitTokenRedeemer where
  toData MintInitToken = Integer (BigInt.fromInt 0)
  toData BurnInitToken = Integer (BigInt.fromInt 1)

instance FromData InitTokenRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just MintInitToken
    | x == BigInt.fromInt 1 = Just BurnInitToken
  fromData _ = Nothing

newtype InitTokenAssetClass = InitTokenAssetClass
  { initTokenCurrencySymbol :: CurrencySymbol
  , initTokenName :: TokenName
  }

derive instance Generic InitTokenAssetClass _

derive newtype instance Eq InitTokenAssetClass

derive newtype instance Show InitTokenAssetClass

instance ToData InitTokenAssetClass where
  toData
    ( InitTokenAssetClass
        { initTokenCurrencySymbol
        , initTokenName
        }
    ) = productToData2 initTokenCurrencySymbol initTokenName

instance FromData InitTokenAssetClass where
  fromData = productFromData2
    \initTokenCurrencySymbol initTokenName ->
      InitTokenAssetClass { initTokenCurrencySymbol, initTokenName }
