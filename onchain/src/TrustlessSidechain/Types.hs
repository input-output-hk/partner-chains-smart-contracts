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
  VersionOracle (..),
  VersionOracleConfig (..),
  VersionOracleDatum (..),
  VersionOraclePolicyRedeemer (..),
) where

import PlutusLedgerApi.Data.V2 (
  BuiltinData (BuiltinData),
  CurrencySymbol,
  ScriptHash,
 )
import PlutusLedgerApi.V1.Data.Value (AssetClass)
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
  { tokenKind :: AssetClass
  -- ^ `tokenKind` is an asset class of tokens that a reserve
  -- UTxO is allowed to store
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

instance ToData ImmutableReserveSettings where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (ImmutableReserveSettings a) = toBuiltinData a

instance FromData ImmutableReserveSettings where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = ImmutableReserveSettings <$> fromBuiltinData x

instance UnsafeFromData ImmutableReserveSettings where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = ImmutableReserveSettings . unsafeFromBuiltinData

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

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
--
-- @since v5.0.0
newtype VersionOracle = VersionOracle
  { scriptId :: Integer
  -- ^ Unique identifier of the validator.
  -- @since v5.0.0
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v5.0.0
instance ToData VersionOracle where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracle {scriptId} =
    toBuiltinData scriptId

-- | @since v5.0.0
instance FromData VersionOracle where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracle <$> fromBuiltinData x

-- | @since v5.0.0
instance UnsafeFromData VersionOracle where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x = VersionOracle $ unsafeFromBuiltinData x

-- | @since v5.0.0
instance Eq VersionOracle where
  VersionOracle s == VersionOracle s' = s == s'

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- 'VersionOracleValidator' script.
data VersionOracleDatum = VersionOracleDatum
  { versionOracle :: VersionOracle
  -- ^ VersionOracle which identifies the script.
  -- @since v6.0.0
  , currencySymbol :: CurrencySymbol
  -- ^ Currency Symbol of the VersioningOraclePolicy tokens.
  -- @since v6.0.0
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v6.0.0
instance ToData VersionOracleDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracleDatum {versionOracle, currencySymbol} =
    productToData2 versionOracle currencySymbol

-- | @since v6.0.0
instance FromData VersionOracleDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 VersionOracleDatum

-- | @since v6.0.0
instance UnsafeFromData VersionOracleDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 VersionOracleDatum

-- | @since v6.0.0
instance Eq VersionOracleDatum where
  VersionOracleDatum vO c == VersionOracleDatum vO' c' = vO == vO' && c == c'

-- | Configuration of the versioning system.  Contains currency symbol of
-- VersionOraclePolicy tokens.  Required to identify versioning tokens that can
-- be trusted.
--
-- @since v5.0.0
newtype VersionOracleConfig = VersionOracleConfig
  { versionOracleCurrencySymbol :: CurrencySymbol
  -- ^ @since v5.0.0
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v5.0.0
instance ToData VersionOracleConfig where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracleConfig {versionOracleCurrencySymbol} =
    toBuiltinData versionOracleCurrencySymbol

-- | @since v5.0.0
instance FromData VersionOracleConfig where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracleConfig <$> fromBuiltinData x

-- | @since v5.0.0
instance UnsafeFromData VersionOracleConfig where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = VersionOracleConfig . unsafeFromBuiltinData

-- | Redeemer for the versioning oracle minting policy that instructs the script
-- whether to mint or burn versioning tokens.
--
-- @since v5.0.0
data VersionOraclePolicyRedeemer
  = -- | Mint versioning tokens from init tokens.  Used during sidechain
    -- initialization.
    -- @since v6.0.0
    InitializeVersionOracle VersionOracle ScriptHash
  | -- | Mint a new versioning token ensuring it contains correct datum and
    -- reference script.
    -- @since v5.0.0
    MintVersionOracle VersionOracle ScriptHash
  | -- | Burn existing versioning token.
    -- @since v5.0.0
    BurnVersionOracle VersionOracle

PlutusTx.makeIsDataIndexed
  ''VersionOraclePolicyRedeemer
  [ ('InitializeVersionOracle, 0)
  , ('MintVersionOracle, 1)
  , ('BurnVersionOracle, 2)
  ]
