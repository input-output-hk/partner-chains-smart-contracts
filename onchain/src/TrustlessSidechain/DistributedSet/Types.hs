{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.DistributedSet.Types (
  Ds (..),
  DsDatum (..),
  Node (..),
  DsConfDatum (..),
  DsConfMint (..),
  DsKeyMint (..),
  Ib (..),
) where

import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  TxOutRef,
  ValidatorHash,
 )
import PlutusTx qualified
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude

{- | Distributed Set (abbr. 'Ds') is the type which parameterizes the validator
 for the distributed set. (See Note [How This All Works]. Moreover, this
 parameterizes the 'mkInsertValidator' and is used as the type which identifies
 the appropriate datum and redeemer type
-}
newtype Ds = Ds
  { -- | The 'CurrencySymbol' which identifies the utxo with 'DsConfDatum'.
    -- |
    -- | @since v4.0.0
    identitySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (FromData, ToData, UnsafeFromData)

-- | 'DsDatum' is the datum in the distributed set. See: Note [How This All Works]
newtype DsDatum = DsDatum
  { -- | @since v4.0.0
    next :: BuiltinByteString
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (Eq, FromData, ToData, UnsafeFromData)

{- | 'Node' is an internal data type of the tree node used in the validator.
 See: Note [How This All Works].
-}
data Node = Node
  { -- | @since v4.0.0
    key :: BuiltinByteString
  , -- | @since v4.0.0
    next :: BuiltinByteString
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

instance Eq Node where
  {-# INLINEABLE (==) #-}
  Node k n == Node k' n' = k == k' && n == n'

-- | @since v4.0.0
instance ToData Node where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Node {..}) = productToData2 key next

-- | @since v4.0.0
instance FromData Node where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 Node

-- | @since v4.0.0
instance UnsafeFromData Node where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 Node

{- | 'DsConfDatum' is the datum which contains the 'CurrencySymbol's of various
 minting policies needed by the distributed set.
-}
data DsConfDatum = DsConfDatum
  { -- | @since v4.0.0
    keyPolicy :: CurrencySymbol
  , -- | @since v4.0.0
    fuelPolicy :: CurrencySymbol
  }
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

instance Eq DsConfDatum where
  {-# INLINEABLE (==) #-}
  DsConfDatum a b == DsConfDatum a' b' = a == a' && b == b'

-- | @since v4.0.0
instance ToData DsConfDatum where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (DsConfDatum {..}) = productToData2 keyPolicy fuelPolicy

-- | @since v4.0.0
instance FromData DsConfDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DsConfDatum

-- | @since v4.0.0
instance UnsafeFromData DsConfDatum where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DsConfDatum

{- | 'Ib' is the insertion buffer (abbr. Ib) where we store which is a fixed
 length "array" of how many new nodes (this is always 2, see 'lengthIb') are
 generated after inserting into a node.
-}
newtype Ib a = Ib {unIb :: (a, a)}
  deriving stock (TSPrelude.Show, TSPrelude.Eq)
  deriving newtype (Eq)

instance TSPrelude.Foldable Ib where
  foldMap f (Ib (a, b)) = f a TSPrelude.<> f b

-- | @since v4.0.0
instance (ToData a) => ToData (Ib a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Ib (x, y)) = productToData2 x y

-- | @since v4.0.0
instance (PlutusTx.FromData a) => PlutusTx.FromData (Ib a) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 (curry Ib)

-- | @since v4.0.0
instance (PlutusTx.UnsafeFromData a) => PlutusTx.UnsafeFromData (Ib a) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 (curry Ib)

{- | 'DsConfMint' is the parameter for the NFT to initialize the distributed
 set. See 'mkDsConfPolicy' for more details.
-}
newtype DsConfMint = DsConfMint
  { -- | @since v4.0.0
    txOutRef :: TxOutRef
  }
  deriving newtype (PlutusTx.FromData, PlutusTx.ToData, PlutusTx.UnsafeFromData)
  deriving stock
    ( -- | @since v4.0.0
      TSPrelude.Eq
    , -- | @since v4.0.0
      TSPrelude.Show
    )

{- | 'DsKeyMint' is the parameter for the minting policy. In particular, the
 'TokenName' of this 'CurrencySymbol' (from 'mkDsKeyPolicy') stores the key of
 the token. See Note [How This All Works] for more details.
-}
data DsKeyMint = DsKeyMint
  { -- | The validator hash that the minting policy
    -- | essentially "forwards" its checks to the validator.
    -- |
    -- | TODO: as an optimization, we can take the 'Address' as a parameter
    -- | instead (since the offchain code will always immediately convert this
    -- | into an 'Address').
    -- |
    -- | @since v4.0.0
    validatorHash :: ValidatorHash
  , -- | The currency symbol to identify a utxo with 'DsConfDatum'
    -- |
    -- | @since v4.0.0
    confCurrencySymbol :: CurrencySymbol
  }
  deriving stock (TSPrelude.Show, TSPrelude.Eq)

-- | @since v4.0.0
instance ToData DsKeyMint where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (DsKeyMint {..}) =
    productToData2 validatorHash confCurrencySymbol

-- | @since v4.0.0
instance FromData DsKeyMint where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 DsKeyMint

-- | @since v4.0.0
instance UnsafeFromData DsKeyMint where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 DsKeyMint

PlutusTx.makeLift ''DsKeyMint
PlutusTx.makeLift ''DsConfDatum
