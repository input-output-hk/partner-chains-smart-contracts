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
import TrustlessSidechain.Governance.Admin as Governance

newtype SidechainParams = SidechainParams
  { genesisUtxo :: TransactionInput
  , -- Governance mechanism.  We temporarily rely on using a single master key
    -- that can authorize any action requiring permission from the governing
    -- committee.
    governanceAuthority :: Governance.GovernanceAuthority
  }

derive instance Generic SidechainParams _

derive instance Newtype SidechainParams _

derive newtype instance Eq SidechainParams

instance ToData SidechainParams where
  toData
    ( SidechainParams
        { genesisUtxo
        , governanceAuthority
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData genesisUtxo
      , toData governanceAuthority
      ]

instance FromData SidechainParams where
  fromData = case _ of
    Constr ix [ gu, ga ] -> do
      guard (ix == BigNum.fromInt 0)
      genesisUtxo <- fromData gu
      governanceAuthority <- fromData ga
      pure $ SidechainParams
        { genesisUtxo
        , governanceAuthority
        }
    _ -> Nothing

instance Show SidechainParams where
  show = genericShow
