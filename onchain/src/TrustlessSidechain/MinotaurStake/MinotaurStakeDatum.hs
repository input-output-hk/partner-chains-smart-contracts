{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | Defines the 'MinotaurStakeDatum' and 'MinotaurStakeRedeemer'
-- types, along with the 'minotaurStakeTokenName'.
module TrustlessSidechain.MinotaurStake.MinotaurStakeDatum where

import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V2.Ledger.Api (
  BuiltinData (BuiltinData),
  PubKeyHash,
  TokenName (TokenName),
 )
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
data MinotaurStakeDatum = MinotaurStakeDatum
  { -- | reward address on partner chain
    -- @since Unreleased
    partnerChainRewardAddress :: BuiltinByteString
  , -- | pub key hash of stake key of the delegator
    -- @since Unreleased
    stakePubKeyHash :: PubKeyHash
  , -- | ID of the stake pool
    -- @since Unreleased
    stakePoolId :: BuiltinByteString
  , -- | currency symbol of the minotaur stake delegation token
    -- @since Unreleased
    stakeCurrencySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since Unreleased
instance ToData MinotaurStakeDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MinotaurStakeDatum {..}) =
    productToData4
      partnerChainRewardAddress
      stakePubKeyHash
      stakePoolId
      stakeCurrencySymbol

-- | @since Unreleased
instance FromData MinotaurStakeDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData4 MinotaurStakeDatum

-- | @since Unreleased
instance UnsafeFromData MinotaurStakeDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData4 MinotaurStakeDatum

-- | module.
minotaurStakeTokenName :: TokenName
minotaurStakeTokenName = TokenName "Minotaur Stake"

-- | MinotaurStakeRedeemer is used to mint and burn MinotaurStake tokens.
--
-- @since v5.0.0
data MinotaurStakeRedeemer
  = -- | Mint a new MinotaurStake token
    -- @since Unreleased
    MintMinotaurStake
  | -- | Burn a MinotaurStake token
    -- @since Unreleased
    BurnMinotaurStake
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

instance ToData MinotaurStakeRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData MintMinotaurStake = BuiltinData (PlutusTx.I 0)
  toBuiltinData BurnMinotaurStake = BuiltinData (PlutusTx.I 1)

instance FromData MinotaurStakeRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> pure MintMinotaurStake
      1 -> pure BurnMinotaurStake
      _ -> Nothing

instance UnsafeFromData MinotaurStakeRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x = do
    case unsafeFromBuiltinData x :: Integer of
      0 -> MintMinotaurStake
      1 -> BurnMinotaurStake
      _ -> error ()
