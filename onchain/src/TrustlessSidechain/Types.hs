{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Types (
  BlockProducerRegistration (..),
  BlockProducerRegistrationMsg (..),
  DParameterValidatorDatum (..),
  EcdsaSecp256k1PubKey (..),
  GovernanceAuthority (GovernanceAuthority),
  PermissionedCandidateKeys (..),
  PermissionedCandidatesPolicyRedeemer (..),
  PermissionedCandidatesValidatorDatum (..),
  PermissionedCandidatesValidatorRedeemer (..),
  PubKey (..),
  SidechainParams (..),
  Signature (..),
  StakeOwnership (..),
  ImmutableReserveSettings (..),
  MutableReserveSettings (..),
  ReserveStats (..),
  ReserveDatum (..),
  ReserveRedeemer (..),
  IlliquidCirculationSupplyRedeemer (..),
) where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (
  BuiltinData (BuiltinData),
  CurrencySymbol,
  LedgerBytes (LedgerBytes),
  POSIXTime,
  TxOutRef,
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

-- * Sidechain Parametrization and general data

newtype GovernanceAuthority = GovernanceAuthority PubKeyHash
  deriving newtype (TSPrelude.Eq, TSPrelude.Ord, TSPrelude.Show, ToData, FromData, UnsafeFromData)

PlutusTx.makeLift ''GovernanceAuthority

-- | Parameters uniquely identifying a sidechain
--
-- = Note
--
-- The 'Data' serializations for this type /cannot/ change.
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisUtxo :: TxOutRef
  -- ^ 'genesisUtxo' is a 'TxOutRef' used to initialize the internal
  -- policies in the side chain (e.g. for the 'UpdateCommitteeHash' endpoint)
  , thresholdNumerator :: Integer
  -- ^ 'thresholdNumerator' is the numerator for the ratio of the committee
  -- needed to sign off committee handovers / merkle roots
  , thresholdDenominator :: Integer
  -- ^ 'thresholdDenominator' is the denominator for the ratio of the
  -- committee needed to sign off committee handovers / merkle roots
  , governanceAuthority :: GovernanceAuthority
  -- ^ 'governanceAuthority' stores credentials of a governing body allowed to
  -- make updates to versioned scripts.  For now we just use a master public
  -- key, whose owner is allowed to make any decisions about script versions.
  --
  -- @since v5.0.0
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeLift ''SidechainParams
PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

-- | @since v4.0.0
makeHasField ''SidechainParams

-- | Compressed DER SECP256k1 public key.
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
newtype EcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey
  { getEcdsaSecp256k1PubKey :: LedgerBytes
  -- ^ @since v4.0.0
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving
    ( -- | @since v4.0.0
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- | @since v5.0.0
makeHasField ''EcdsaSecp256k1PubKey

-- | Ed25519 public key
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
newtype PubKey = PubKey
  -- TODO: rename to Ed25519PubKEy
  { getPubKey :: LedgerBytes
  -- ^ @since v4.0.0
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving
    ( -- | @since v4.0.0
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- | Ed25519 signature
-- = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
newtype Signature = Signature
  -- TODO: rename to Ed25519Signature
  { getSignature :: LedgerBytes
  -- ^ @since v4.0.0
  }
  deriving stock (TSPrelude.Eq, TSPrelude.Ord)
  deriving newtype
    ( Eq
    , Ord
    , ToData
    , FromData
    , UnsafeFromData
    )
  deriving
    ( -- | @since v4.0.0
      IsString
    , TSPrelude.Show
    )
    via LedgerBytes

-- * Committee Candidate Validator data

{- Sum type distinguishing different Stake ownership models
 Ada based staking requires the SPO public key and the signature on
 the @BlockProducerRegistrationMsg@, while a native token based staking model
 only requires the own Cardano payment public key hash

-}
data StakeOwnership
  = -- | Ada stake based configuration comprises the SPO public key and signature
    AdaBasedStaking PubKey Signature
  | -- | Token based staking configuration
    TokenBasedStaking
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed
  ''StakeOwnership
  [ ('AdaBasedStaking, 0)
  , ('TokenBasedStaking, 1)
  ]

{-
 The 'Data' serializations for this type /cannot/ change.
-}
data BlockProducerRegistration = BlockProducerRegistration
  { stakeOwnership :: StakeOwnership
  -- ^ Verification keys required by the stake ownership model
  -- | @since v4.0.0
  , sidechainPubKey :: LedgerBytes
  -- ^ public key in the sidechain's desired format
  , sidechainSignature :: Signature
  -- ^ Signature of the sidechain
  -- | @since v4.0.0
  , inputUtxo :: TxOutRef
  -- ^ A UTxO that must be spent by th@ext:haskell.haskelltransaction
  -- | @since v4.0.0
  , ownPkh :: PubKeyHash
  -- ^ Owner public key hash
  -- | @since v4.0.0
  , auraKey :: LedgerBytes
  -- ^ Sidechain authority discovery key
  -- | @since v5.0.0
  , grandpaKey :: LedgerBytes
  -- ^ Sidechain grandpa key
  -- | @since v5.0.0
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

-- | @since v4.0.0
makeHasField ''BlockProducerRegistration

-- | = Important note
--
-- The 'Data' serializations for this type /cannot/ change.
data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { sidechainParams :: SidechainParams
  , sidechainPubKey :: LedgerBytes
  , inputUtxo :: TxOutRef
  -- ^ A UTxO that must be spent by the transaction
  -- | @since v4.0.0
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

-- | @since v4.0.0
makeHasField ''BlockProducerRegistrationMsg

-- | 'DParameterValidatorDatum' stores the ratio of permissioned candidates.  This
-- ratio is represented as a pair of integers - permissionedCandidatesCount and
-- registeredCandidatesCount.
--
-- @since v5.0.0
data DParameterValidatorDatum = DParameterValidatorDatum
  { permissionedCandidatesCount :: Integer
  -- ^ @since v5.0.0
  , registeredCandidatesCount :: Integer
  -- ^ @since v5.0.0
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
makeHasField ''DParameterValidatorDatum

-- | @since v5.0.0
instance ToData DParameterValidatorDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData
    ( DParameterValidatorDatum
        permissionedCandidatesCount
        registeredCandidatesCount
      ) =
      productToData2 permissionedCandidatesCount registeredCandidatesCount

-- | @since v5.0.0
instance FromData DParameterValidatorDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DParameterValidatorDatum

-- | @since v5.0.0
instance UnsafeFromData DParameterValidatorDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DParameterValidatorDatum

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

-- | 'PermissionedCandidateKeys' stores the keys of some permissioned candiate.
--
-- @since v5.0.0
data PermissionedCandidateKeys = PermissionedCandidateKeys
  { sidechainKey :: LedgerBytes
  -- ^ @since v5.0.0
  , auraKey :: LedgerBytes
  -- ^ @since v5.0.0
  , grandpaKey :: LedgerBytes
  -- ^ @since v5.0.0
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )

-- | @since v5.0.0
instance ToData PermissionedCandidateKeys where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (PermissionedCandidateKeys s a g) =
    productToData3 s a g

-- | @since v5.0.0
instance FromData PermissionedCandidateKeys where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData3 PermissionedCandidateKeys

-- | @since v5.0.0
instance UnsafeFromData PermissionedCandidateKeys where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData3 PermissionedCandidateKeys

-- | @since v5.0.0
makeHasField ''PermissionedCandidateKeys

-- | 'PermissionedCandidatesValidatorDatum' stores a list of permissioned
--   candidates' keys.
--
-- @since v5.0.0
newtype PermissionedCandidatesValidatorDatum = PermissionedCandidatesValidatorDatum
  { candidates :: [PermissionedCandidateKeys]
  }
  deriving stock
    ( TSPrelude.Eq
    , TSPrelude.Show
    )
  deriving newtype (ToData, FromData, UnsafeFromData)

-- | @since v5.0.0
makeHasField ''PermissionedCandidatesValidatorDatum

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

makeHasField ''ImmutableReserveSettings

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

makeHasField ''MutableReserveSettings

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

makeHasField ''ReserveStats

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

makeHasField ''ReserveDatum

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
