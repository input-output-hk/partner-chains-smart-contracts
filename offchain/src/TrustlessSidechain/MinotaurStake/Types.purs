module TrustlessSidechain.MinotaurStake.Types
  ( MinotaurStakeDatum(MinotaurStakeDatum)
  , MinotaurStakePolicyRedeemer(MintMinotaurStake, BurnMinotaurStake)
  ) where

import Contract.Prelude

import Contract.Address (PubKeyHash)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer)
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import Data.BigInt as BigInt
import TrustlessSidechain.Utils.Data
  ( productFromData4
  , productToData4
  )

data MinotaurStakePolicyRedeemer
  = MintMinotaurStake
  | BurnMinotaurStake

derive instance Eq MinotaurStakePolicyRedeemer

derive instance Generic MinotaurStakePolicyRedeemer _

instance Show MinotaurStakePolicyRedeemer where
  show = genericShow

instance ToData MinotaurStakePolicyRedeemer where
  toData MintMinotaurStake = Integer (BigInt.fromInt 0)
  toData BurnMinotaurStake = Integer (BigInt.fromInt 1)

instance FromData MinotaurStakePolicyRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ MintMinotaurStake
    | x == BigInt.fromInt 1 = Just $ BurnMinotaurStake
  fromData _ = Nothing

data MinotaurStakeDatum = MinotaurStakeDatum
  { partnerChainRewardAddress ∷ ByteArray
  , stakePubKeyHash ∷ PubKeyHash
  , stakePoolId ∷ ByteArray
  , stakeCurrencySymbol ∷ CurrencySymbol
  }

derive instance Eq MinotaurStakeDatum
derive instance Ord MinotaurStakeDatum

derive instance Generic MinotaurStakeDatum _

instance Show MinotaurStakeDatum where
  show = genericShow

instance ToData MinotaurStakeDatum where
  toData
    ( MinotaurStakeDatum
        { partnerChainRewardAddress
        , stakePubKeyHash
        , stakePoolId
        , stakeCurrencySymbol
        }
    ) =
    productToData4
      partnerChainRewardAddress
      stakePubKeyHash
      stakePoolId
      stakeCurrencySymbol

instance FromData MinotaurStakeDatum where
  fromData =
    productFromData4 $
      \partnerChainRewardAddress stakePubKeyHash stakePoolId stakeCurrencySymbol →
        MinotaurStakeDatum
          { partnerChainRewardAddress
          , stakePubKeyHash
          , stakePoolId
          , stakeCurrencySymbol
          }
