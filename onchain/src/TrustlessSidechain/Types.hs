{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Types (
  PermissionedCandidatesPolicyRedeemer (..),
  PermissionedCandidatesValidatorRedeemer (..),
  ImmutableReserveSettings (..),
  MutableReserveSettings (..),
  ReserveStats (..),
  ReserveDatum (..),
  ReserveRedeemer (..),
  IlliquidCirculationSupplyRedeemer (..),
  VersionedGenericDatum (..),
) where

import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (
  BuiltinData (BuiltinData),
  CurrencySymbol,
  POSIXTime,
 )
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude

-- Note [Roundtrip tests]
-- ~~~~~~~~~~~~~~~~~~~~~~
--
-- Whenever new definitions in this module are added, i.e. either a new data
-- type or a new data constructor is created, corresponding roundtrip tests need
-- to be updated.  These tests reside in Test.TrustlessSidechain.Types module in
-- `tests/` directory and test correctness of serialization and deserialization
-- using `ToData` and `FromData` instances.  Importantly, same tests are
-- performed in the substrate-node to ensure interoperability with
-- trustless-sidechain.

-- * Committee Candidate Validator data

-- | 'PermissionedCandidatesPolicyRedeemer' signals whether transaction is supposed to mint or
-- burn PermissionedCandidates tokens
--
-- @since v5.0.0
data PermissionedCandidatesPolicyRedeemer
  = -- | @since v5.0.0
    PermissionedCandidatesMint
  | -- | @since v5.0.0
    PermissionedCandidatesBurn
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
instance ToData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData PermissionedCandidatesMint = BuiltinData $ PlutusTx.I 0
  toBuiltinData PermissionedCandidatesBurn = BuiltinData $ PlutusTx.I 1

-- | @since v5.0.0
instance FromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just PermissionedCandidatesMint
      1 -> Just PermissionedCandidatesBurn
      _ -> Nothing

-- | @since v5.0.0
instance UnsafeFromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> PermissionedCandidatesMint
          1 -> PermissionedCandidatesBurn
          _ -> error ()

-- | 'PermissionedCandidatesValidatorRedeemer' signals whether transaction is
-- supposed to update the list of permissioned candidates or remove the list
-- altogether.
--
-- @since v5.0.0
data PermissionedCandidatesValidatorRedeemer
  = -- | @since v5.0.0
    UpdatePermissionedCandidates
  | -- | @since v5.0.0
    RemovePermissionedCandidates
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
instance ToData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData UpdatePermissionedCandidates = BuiltinData $ PlutusTx.I 0
  toBuiltinData RemovePermissionedCandidates = BuiltinData $ PlutusTx.I 1

-- | @since v5.0.0
instance FromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just UpdatePermissionedCandidates
      1 -> Just RemovePermissionedCandidates
      _ -> Nothing

-- | @since v5.0.0
instance UnsafeFromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> UpdatePermissionedCandidates
          1 -> RemovePermissionedCandidates
          _ -> error ()

data ImmutableReserveSettings = ImmutableReserveSettings
  { t0 :: POSIXTime
  -- ^ `t0` is a POSIX time of a reserve UTxO initialization
  , tokenKind :: AssetClass
  -- ^ `tokenKind` is an asset class of tokens that a reserve
  -- UTxO is allowed to store
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData ImmutableReserveSettings where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (ImmutableReserveSettings s a) =
    productToData2 s a

instance FromData ImmutableReserveSettings where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 ImmutableReserveSettings

instance UnsafeFromData ImmutableReserveSettings where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 ImmutableReserveSettings

data MutableReserveSettings = MutableReserveSettings
  { vFunctionTotalAccrued :: CurrencySymbol
  -- ^ `vFunctionTotalAccrued` is a currency symbol of a minting policy
  -- that dictates the upper bound on the number of `tokenKind` tokens that
  -- can be transferred from a reserve utxo to an illiquid circulation supply
  -- from `t0` till now
  , incentiveAmount :: Integer
  -- ^ The amount of `tokenKind` the user is allowed to claim when releasing
  -- money from the reserve
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData MutableReserveSettings where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MutableReserveSettings vt i) =
    productToData2 vt i

instance FromData MutableReserveSettings where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 MutableReserveSettings

instance UnsafeFromData MutableReserveSettings where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 MutableReserveSettings

newtype ReserveStats = ReserveStats
  { tokenTotalAmountTransferred :: Integer
  -- ^ `tokenTotalAmountTransferred` is the total number
  -- of tokens that already have been transferred from a reserve utxo
  -- to an illiquid circulation supply
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )
  deriving newtype (ToData, FromData, UnsafeFromData, Eq)

data ReserveDatum = ReserveDatum
  { immutableSettings :: ImmutableReserveSettings
  , mutableSettings :: MutableReserveSettings
  , stats :: ReserveStats
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData ReserveDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (ReserveDatum s a g) =
    productToData3 s a g

instance FromData ReserveDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 ReserveDatum

instance UnsafeFromData ReserveDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 ReserveDatum

data ReserveRedeemer
  = DepositToReserve
  | TransferToIlliquidCirculationSupply
  | UpdateReserve
  | Handover
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed
  ''ReserveRedeemer
  [ ('DepositToReserve, 0)
  , ('TransferToIlliquidCirculationSupply, 1)
  , ('UpdateReserve, 2)
  , ('Handover, 3)
  ]

data IlliquidCirculationSupplyRedeemer
  = DepositMoreToSupply
  | WithdrawFromSupply
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData IlliquidCirculationSupplyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData DepositMoreToSupply = BuiltinData $ PlutusTx.I 0
  toBuiltinData WithdrawFromSupply = BuiltinData $ PlutusTx.I 1

instance FromData IlliquidCirculationSupplyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just DepositMoreToSupply
      1 -> Just WithdrawFromSupply
      _ -> Nothing

instance UnsafeFromData IlliquidCirculationSupplyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          0 -> DepositMoreToSupply
          1 -> WithdrawFromSupply
          _ -> error ()

data VersionedGenericDatum a = VersionedGenericDatum
  { datum :: a
  , genericData :: BuiltinData
  , version :: Integer
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance (ToData a) => ToData (VersionedGenericDatum a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (VersionedGenericDatum d g v) =
    productToData3 d g v

instance (FromData a) => FromData (VersionedGenericDatum a) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 VersionedGenericDatum

instance (UnsafeFromData a) => UnsafeFromData (VersionedGenericDatum a) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 VersionedGenericDatum
