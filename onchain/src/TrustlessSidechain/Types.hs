{-# LANGUAGE TemplateHaskell #-}
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
import PlutusTx.Prelude
import TrustlessSidechain.EncodeHelpers

import Data.Eq qualified as Haskell
import Text.Show qualified as Haskell

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

{- | 'PermissionedCandidatesPolicyRedeemer' signals whether transaction is supposed to mint or
burn PermissionedCandidates tokens
-}
data PermissionedCandidatesPolicyRedeemer
  = PermissionedCandidatesMint
  | PermissionedCandidatesBurn
  deriving stock (Haskell.Eq, Haskell.Show)

instance ToData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData PermissionedCandidatesMint = BuiltinData $ PlutusTx.I 0
  toBuiltinData PermissionedCandidatesBurn = BuiltinData $ PlutusTx.I 1

instance FromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just PermissionedCandidatesMint
      1 -> Just PermissionedCandidatesBurn
      _ -> Nothing

instance UnsafeFromData PermissionedCandidatesPolicyRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          i | i == 0 -> PermissionedCandidatesMint
          i | i == 1 -> PermissionedCandidatesBurn
          _ -> error ()

{- | 'PermissionedCandidatesValidatorRedeemer' signals whether transaction is
supposed to update the list of permissioned candidates or remove the list
altogether.
-}
data PermissionedCandidatesValidatorRedeemer
  = UpdatePermissionedCandidates
  | RemovePermissionedCandidates
  deriving stock (Haskell.Eq, Haskell.Show)

instance ToData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData UpdatePermissionedCandidates = BuiltinData $ PlutusTx.I 0
  toBuiltinData RemovePermissionedCandidates = BuiltinData $ PlutusTx.I 1

instance FromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = do
    integerValue <- fromBuiltinData x
    case integerValue :: Integer of
      0 -> Just UpdatePermissionedCandidates
      1 -> Just RemovePermissionedCandidates
      _ -> Nothing

instance UnsafeFromData PermissionedCandidatesValidatorRedeemer where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x =
    let integerValue = unsafeFromBuiltinData x
     in case integerValue :: Integer of
          i | i == 0 -> UpdatePermissionedCandidates
          i | i == 1 -> RemovePermissionedCandidates
          _ -> error ()

data ImmutableReserveSettings = ImmutableReserveSettings
  { tokenKind :: AssetClass
  -- ^ `tokenKind` is an asset class of tokens that a reserve
  -- UTxO is allowed to store
  }
  deriving stock (Haskell.Eq, Haskell.Show)

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
  deriving stock (Haskell.Eq, Haskell.Show)

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
  deriving stock (Haskell.Eq, Haskell.Show)
  deriving newtype (ToData, FromData, UnsafeFromData, Eq)

data ReserveDatum = ReserveDatum
  { immutableSettings :: ImmutableReserveSettings
  , mutableSettings :: MutableReserveSettings
  , stats :: ReserveStats
  }
  deriving stock (Haskell.Eq, Haskell.Show)

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
  deriving stock (Haskell.Eq, Haskell.Show)

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
  deriving stock (Haskell.Eq, Haskell.Show)

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
          i | i == 0 -> DepositMoreToSupply
          i | i == 1 -> WithdrawFromSupply
          _ -> error ()

data VersionedGenericDatum a = VersionedGenericDatum
  { datum :: a
  , genericData :: BuiltinData
  , version :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show)

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

{- | Datum attached to 'VersionOraclePolicy' tokens stored on the
'VersionOracleValidator' script.
-}
newtype VersionOracle = VersionOracle
  { scriptId :: Integer
  -- ^ Unique identifier of the validator.
  }
  deriving stock (Haskell.Show, Haskell.Eq)

instance ToData VersionOracle where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracle {scriptId} =
    toBuiltinData scriptId

instance FromData VersionOracle where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracle <$> fromBuiltinData x

instance UnsafeFromData VersionOracle where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData x = VersionOracle $ unsafeFromBuiltinData x

instance Eq VersionOracle where
  VersionOracle s == VersionOracle s' = s == s'

{- | Datum attached to 'VersionOraclePolicy' tokens stored on the
'VersionOracleValidator' script.
-}
data VersionOracleDatum = VersionOracleDatum
  { versionOracle :: VersionOracle
  -- ^ VersionOracle which identifies the script.
  , currencySymbol :: CurrencySymbol
  -- ^ Currency Symbol of the VersioningOraclePolicy tokens.
  }
  deriving stock (Haskell.Show, Haskell.Eq)

instance ToData VersionOracleDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracleDatum {versionOracle, currencySymbol} =
    productToData2 versionOracle currencySymbol

instance FromData VersionOracleDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 VersionOracleDatum

instance UnsafeFromData VersionOracleDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 VersionOracleDatum

instance Eq VersionOracleDatum where
  VersionOracleDatum vO c == VersionOracleDatum vO' c' = vO == vO' && c == c'

{- | Configuration of the versioning system.  Contains currency symbol of
VersionOraclePolicy tokens.  Required to identify versioning tokens that can
be trusted.
-}
newtype VersionOracleConfig = VersionOracleConfig
  { versionOracleCurrencySymbol :: CurrencySymbol
  -- ^ @since v5.0.0
  }
  deriving stock (Haskell.Show, Haskell.Eq)

instance ToData VersionOracleConfig where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData VersionOracleConfig {versionOracleCurrencySymbol} =
    toBuiltinData versionOracleCurrencySymbol

instance FromData VersionOracleConfig where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData x = VersionOracleConfig <$> fromBuiltinData x

instance UnsafeFromData VersionOracleConfig where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = VersionOracleConfig . unsafeFromBuiltinData

{- | Redeemer for the versioning oracle minting policy that instructs the script
whether to mint or burn versioning tokens.
-}
data VersionOraclePolicyRedeemer
  = -- | Mint versioning tokens from init tokens.  Used during sidechain
    -- initialization.
    InitializeVersionOracle VersionOracle ScriptHash
  | -- | Mint a new versioning token ensuring it contains correct datum and
    -- reference script.
    MintVersionOracle VersionOracle ScriptHash
  | -- | Burn existing versioning token.
    BurnVersionOracle VersionOracle

PlutusTx.makeIsDataIndexed
  ''VersionOraclePolicyRedeemer
  [ ('InitializeVersionOracle, 0)
  , ('MintVersionOracle, 1)
  , ('BurnVersionOracle, 2)
  ]
