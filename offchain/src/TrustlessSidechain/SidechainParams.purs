module TrustlessSidechain.SidechainParams where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Transaction (TransactionInput)
import Control.Alternative (guard)

newtype SidechainParams = SidechainParams
  { genesisUtxo :: TransactionInput
  }

derive instance Generic SidechainParams _

derive instance Newtype SidechainParams _

derive newtype instance Eq SidechainParams

instance ToData SidechainParams where
  toData
    ( SidechainParams
        { genesisUtxo
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData genesisUtxo
      ]

instance FromData SidechainParams where
  fromData = case _ of
    Constr ix [ gu ] -> do
      guard (ix == BigNum.fromInt 0)
      genesisUtxo <- fromData gu
      pure $ SidechainParams
        { genesisUtxo
        }
    _ -> Nothing

instance Show SidechainParams where
  show = genericShow
