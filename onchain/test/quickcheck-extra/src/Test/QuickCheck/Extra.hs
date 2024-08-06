{-# LANGUAGE MultiWayIf #-}

-- | Module: Test.QuickCheck.Extra
-- Description: Some improved versions and helpers for QuickCheck functions
-- Copyright: (C) MLabs 2023
-- Mainainter: Koz Ross (koz@mlabs.city)
-- Portability: GHC only
--
-- Some functions designed to supercede equivalent functionality from
-- QuickCheck, for reasons of efficiency or safety.
module Test.QuickCheck.Extra (
  -- * Generators
  suchThat,
  suchThatMap,
  suchThatRetrying,
  suchThatMapRetrying,
  sublistOf,

  -- * Wrappers
  ArbitraryBytes (..),
  ArbitraryTxOutRef (..),
  ArbitraryCurrencySymbol (..),
  ArbitraryScriptHash (..),
  ArbitraryPubKeyHash (..),
  ArbitraryAddress (..),
  ArbitraryCredential (..),
  ArbitraryStakingCredential (..),
  DA (..),
  ArbitraryTokenName (..),
  ArbitraryAssetClass (..),
) where

import Data.Bits (
  countTrailingZeros,
  finiteBitSize,
  unsafeShiftR,
 )
import Data.List (drop)
import GHC.Err (undefined)
import GHC.Exts (fromList, toList)
import PlutusLedgerApi.V1.Value (
  AssetClass (AssetClass),
 )
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  LedgerBytes (LedgerBytes),
  PubKeyHash (PubKeyHash),
  ScriptHash (..),
  StakingCredential (StakingHash, StakingPtr),
  TokenName (TokenName),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import Test.QuickCheck (
  ASCIIString (ASCIIString),
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonNegative (NonNegative),
  functionMap,
  liftArbitrary,
  oneof,
  variant,
  vectorOf,
 )
import Test.QuickCheck.Gen (
  Gen,
  chooseWord64,
  elements,
  resize,
  sized,
 )
import Test.QuickCheck.Poly (A (A))
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.PlutusPrelude qualified as PTPrelude

-- | Wrapper for 'AssetClass' to provide QuickCheck instances. Currently will
-- not generate the ADA asset class.
--
-- @since v4.0.0
newtype ArbitraryAssetClass = ArbitraryAssetClass AssetClass
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via AssetClass
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | @since v4.0.0
instance Arbitrary ArbitraryAssetClass where
  arbitrary =
    ArbitraryAssetClass . AssetClass <$> do
      ArbitraryCurrencySymbol sym <- arbitrary
      ArbitraryTokenName tok <- arbitrary
      pure (sym, tok)
  shrink (ArbitraryAssetClass (AssetClass (sym, tok))) =
    ArbitraryAssetClass . AssetClass <$> do
      ArbitraryCurrencySymbol sym' <- shrink (ArbitraryCurrencySymbol sym)
      ArbitraryTokenName tok' <- shrink (ArbitraryTokenName tok)
      pure (sym', tok')

-- | @since v4.0.0
instance CoArbitrary ArbitraryAssetClass where
  coarbitrary (ArbitraryAssetClass (AssetClass (sym, tok))) =
    coarbitrary (ArbitraryCurrencySymbol sym)
      . coarbitrary (ArbitraryTokenName tok)

-- | @since v4.0.0
instance Function ArbitraryAssetClass where
  function = functionMap into outOf
    where
      into ::
        ArbitraryAssetClass ->
        (ArbitraryCurrencySymbol, ArbitraryTokenName)
      into (ArbitraryAssetClass (AssetClass (sym, tok))) =
        (ArbitraryCurrencySymbol sym, ArbitraryTokenName tok)
      outOf ::
        (ArbitraryCurrencySymbol, ArbitraryTokenName) ->
        ArbitraryAssetClass
      outOf (ArbitraryCurrencySymbol sym, ArbitraryTokenName tok) =
        ArbitraryAssetClass (AssetClass (sym, tok))

-- | Wrapper for 'TokenName' to provide QuickCheck instances. Currently only
-- generates those 'TokenName's that correspond to ASCII strings; this is
-- somewhat limited, but anything more would complicate the shrinker too much,
-- as it would require re-encoding.
--
-- @since v4.0.0
newtype ArbitraryTokenName = ArbitraryTokenName TokenName
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via TokenName
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | @since v4.0.0
instance Arbitrary ArbitraryTokenName where
  arbitrary =
    ArbitraryTokenName . TokenName <$> do
      ASCIIString name <- arbitrary
      let asBS = fromString @ByteString name
      pure . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString $ asBS
  shrink (ArbitraryTokenName (TokenName bbs)) =
    ArbitraryTokenName . TokenName <$> do
      let asList = toList . PTPrelude.fromBuiltin $ bbs
      shrunk <- shrink asList
      let asBS = fromList @ByteString shrunk
      pure . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString $ asBS

-- | @since v4.0.0
instance CoArbitrary ArbitraryTokenName where
  coarbitrary (ArbitraryTokenName (TokenName bbs)) =
    coarbitrary . toList . PTPrelude.fromBuiltin $ bbs

-- | @since v4.0.0
instance Function ArbitraryTokenName where
  function = functionMap into outOf
    where
      into :: ArbitraryTokenName -> [Word8]
      into (ArbitraryTokenName (TokenName bbs)) =
        toList . PTPrelude.fromBuiltin $ bbs
      outOf :: [Word8] -> ArbitraryTokenName
      outOf =
        ArbitraryTokenName
          . TokenName
          . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
          . fromList @ByteString

-- | Similar to 'A', but with 'Data'-related instances included.
--
-- @since v4.0.0
newtype DA = DA A
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Show
    , -- | @since v4.0.0
      Arbitrary
    , -- | @since v4.0.0
      CoArbitrary
    )
    via A
  deriving
    ( -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    , -- | @since v4.0.0
      PTPrelude.ToData
    , -- | @since v4.0.0
      PTPrelude.FromData
    , -- | @since v4.0.0
      PTPrelude.UnsafeFromData
    )
    via Integer

-- | @since v4.0.0
instance Function DA where
  function = functionMap (coerce @DA @A) coerce

-- | Wrapper for 'StakingCredential' to provide Quickcheck instances.
--
-- @since v4.0.0
newtype ArbitraryStakingCredential = ArbitraryStakingCredential StakingCredential
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    )
    via StakingCredential
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | @since v4.0.0
instance Arbitrary ArbitraryStakingCredential where
  arbitrary = ArbitraryStakingCredential <$> oneof [sh, sp]
    where
      sh :: Gen StakingCredential
      sh =
        StakingHash <$> do
          ArbitraryCredential cred <- arbitrary
          pure cred
      sp :: Gen StakingCredential
      sp = StakingPtr <$> go <*> go <*> go
      -- Based on documentation bounding this to Word64
      go :: Gen Integer
      go = fromIntegral <$> chooseWord64 (minBound, maxBound)
  shrink (ArbitraryStakingCredential sc) =
    ArbitraryStakingCredential <$> case sc of
      StakingHash cred ->
        StakingHash <$> do
          ArbitraryCredential cred' <- shrink (ArbitraryCredential cred)
          pure cred'
      StakingPtr i j k -> do
        i' <- shrink i
        j' <- shrink j
        k' <- shrink k
        guard (i' >= 0)
        guard (j' >= 0)
        guard (k' >= 0)
        pure . StakingPtr i' j' $ k'

-- | @since v4.0.0
instance CoArbitrary ArbitraryStakingCredential where
  coarbitrary (ArbitraryStakingCredential sc) = case sc of
    StakingHash cred -> variant (0 :: Int) . coarbitrary (ArbitraryCredential cred)
    StakingPtr i j k -> variant (1 :: Int) . coarbitrary i . coarbitrary j . coarbitrary k

-- | @since v4.0.0
instance Function ArbitraryStakingCredential where
  function = functionMap into outOf
    where
      into :: ArbitraryStakingCredential -> Either ArbitraryCredential (Word64, Word64, Word64)
      into (ArbitraryStakingCredential sc) = case sc of
        StakingHash cred -> Left . ArbitraryCredential $ cred
        StakingPtr i j k -> Right (fromIntegral i, fromIntegral j, fromIntegral k)
      outOf :: Either ArbitraryCredential (Word64, Word64, Word64) -> ArbitraryStakingCredential
      outOf =
        ArbitraryStakingCredential . \case
          Left (ArbitraryCredential cred) -> StakingHash cred
          Right (i, j, k) -> StakingPtr (fromIntegral i) (fromIntegral j) (fromIntegral k)

-- | Wrapper for 'Credential' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryCredential = ArbitraryCredential Credential
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    )
    via Credential
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | @since v4.0.0
instance Arbitrary ArbitraryCredential where
  arbitrary = ArbitraryCredential <$> oneof [pkc, sc]
    where
      pkc :: Gen Credential
      pkc =
        PubKeyCredential <$> do
          ArbitraryPubKeyHash pkh <- arbitrary
          pure pkh
      sc :: Gen Credential
      sc =
        ScriptCredential <$> do
          ArbitraryScriptHash vh <- arbitrary
          pure vh
  shrink (ArbitraryCredential cred) =
    ArbitraryCredential <$> case cred of
      PubKeyCredential pkh ->
        PubKeyCredential <$> do
          ArbitraryPubKeyHash pkh' <- shrink (ArbitraryPubKeyHash pkh)
          pure pkh'
      ScriptCredential vh ->
        ScriptCredential <$> do
          ArbitraryScriptHash vh' <- shrink (ArbitraryScriptHash vh)
          pure vh'

-- | @since v4.0.0
instance CoArbitrary ArbitraryCredential where
  coarbitrary (ArbitraryCredential cred) = case cred of
    PubKeyCredential pkh -> variant (0 :: Int) . coarbitrary (ArbitraryPubKeyHash pkh)
    ScriptCredential vh -> variant (1 :: Int) . coarbitrary (ArbitraryScriptHash vh)

-- | @since v4.0.0
instance Function ArbitraryCredential where
  function = functionMap into outOf
    where
      into :: ArbitraryCredential -> Either ArbitraryPubKeyHash ArbitraryScriptHash
      into (ArbitraryCredential cred) = case cred of
        PubKeyCredential pkh -> Left (ArbitraryPubKeyHash pkh)
        ScriptCredential vh -> Right (ArbitraryScriptHash vh)
      outOf :: Either ArbitraryPubKeyHash ArbitraryScriptHash -> ArbitraryCredential
      outOf =
        ArbitraryCredential . \case
          Left (ArbitraryPubKeyHash pkh) -> PubKeyCredential pkh
          Right (ArbitraryScriptHash vh) -> ScriptCredential vh

-- | Wrapper for 'Address' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryAddress = ArbitraryAddress Address
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    )
    via Address
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as it wouldn't make sense to.
--
-- @since v4.0.0
instance Arbitrary ArbitraryAddress where
  arbitrary =
    ArbitraryAddress <$> do
      ArbitraryCredential cred <- arbitrary
      scred <- liftArbitrary arbitrary
      pure . Address cred $ case scred of
        Nothing -> Nothing
        Just (ArbitraryStakingCredential sc) -> Just sc

-- | @since v4.0.0
instance CoArbitrary ArbitraryAddress where
  coarbitrary (ArbitraryAddress (Address cred scred)) =
    let scred' = fmap ArbitraryStakingCredential scred
     in coarbitrary (ArbitraryCredential cred) . coarbitrary scred'

-- | @since v4.0.0
instance Function ArbitraryAddress where
  function = functionMap into outOf
    where
      into ::
        ArbitraryAddress ->
        (ArbitraryCredential, Maybe ArbitraryStakingCredential)
      into (ArbitraryAddress (Address cred scred)) =
        (ArbitraryCredential cred, fmap ArbitraryStakingCredential scred)
      outOf ::
        (ArbitraryCredential, Maybe ArbitraryStakingCredential) ->
        ArbitraryAddress
      outOf (ArbitraryCredential cred, scred) =
        ArbitraryAddress . Address cred $ case scred of
          Nothing -> Nothing
          Just (ArbitraryStakingCredential scred') -> Just scred'

-- | Wrapper for 'PubKeyHash' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryPubKeyHash = ArbitraryPubKeyHash PubKeyHash
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via PubKeyHash
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as it doesn't make much sense to.
--
-- @since v4.0.0
instance Arbitrary ArbitraryPubKeyHash where
  arbitrary =
    ArbitraryPubKeyHash
      . PubKeyHash
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 28 arbitrary

-- | @since v4.0.0
instance CoArbitrary ArbitraryPubKeyHash where
  coarbitrary (ArbitraryPubKeyHash (PubKeyHash bbs)) =
    coarbitrary . toList . PTPrelude.fromBuiltin $ bbs

-- | @since v4.0.0
instance Function ArbitraryPubKeyHash where
  function = functionMap into outOf
    where
      into :: ArbitraryPubKeyHash -> [Word8]
      into (ArbitraryPubKeyHash (PubKeyHash bbs)) =
        toList . PTPrelude.fromBuiltin $ bbs
      outOf :: [Word8] -> ArbitraryPubKeyHash
      outOf =
        ArbitraryPubKeyHash
          . PubKeyHash
          . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
          . fromList @ByteString

-- | Wrapper for 'CurrencySymbol' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryCurrencySymbol = ArbitraryCurrencySymbol CurrencySymbol
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via CurrencySymbol
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | This does /not/ generate the ADA symbol. Does not shrink (it wouldn't make
-- much sense to).
--
-- @since v4.0.0
instance Arbitrary ArbitraryCurrencySymbol where
  arbitrary =
    ArbitraryCurrencySymbol
      . CurrencySymbol
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 28 arbitrary

-- | @since v4.0.0
instance CoArbitrary ArbitraryCurrencySymbol where
  coarbitrary (ArbitraryCurrencySymbol (CurrencySymbol bbs)) =
    coarbitrary . toList . PTPrelude.fromBuiltin $ bbs

-- | @since v4.0.0
instance Function ArbitraryCurrencySymbol where
  function = functionMap into outOf
    where
      into :: ArbitraryCurrencySymbol -> [Word8]
      into (ArbitraryCurrencySymbol (CurrencySymbol bbs)) =
        toList . PTPrelude.fromBuiltin $ bbs
      outOf :: [Word8] -> ArbitraryCurrencySymbol
      outOf =
        ArbitraryCurrencySymbol
          . CurrencySymbol
          . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
          . fromList @ByteString

-- | Wrapper for 'ScriptHash' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryScriptHash = ArbitraryScriptHash ScriptHash
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via ScriptHash
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink (it wouldn't make
-- much sense to).
--
-- @since v4.0.0
instance Arbitrary ArbitraryScriptHash where
  arbitrary =
    ArbitraryScriptHash
      . ScriptHash
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 28 arbitrary

-- | @since v4.0.0
instance CoArbitrary ArbitraryScriptHash where
  coarbitrary (ArbitraryScriptHash (ScriptHash bbs)) =
    coarbitrary . toList . PTPrelude.fromBuiltin $ bbs

-- | @since v4.0.0
instance Function ArbitraryScriptHash where
  function = functionMap into outOf
    where
      into :: ArbitraryScriptHash -> [Word8]
      into (ArbitraryScriptHash (ScriptHash bbs)) =
        toList . PTPrelude.fromBuiltin $ bbs
      outOf :: [Word8] -> ArbitraryScriptHash
      outOf =
        ArbitraryScriptHash
          . ScriptHash
          . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
          . fromList @ByteString

-- | Wrapper for 'TxId' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryTxId = ArbitraryTxId TxId
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via TxId
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as this is a fixed-width hash.
--
-- @since v4.0.0
instance Arbitrary ArbitraryTxId where
  arbitrary =
    ArbitraryTxId <$> do
      xs <- vectorOf 28 arbitrary
      let bs = fromList @ByteString xs
      pure . TxId . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString $ bs

-- | @since v4.0.0
instance CoArbitrary ArbitraryTxId where
  coarbitrary (ArbitraryTxId (TxId bbs)) =
    coarbitrary . toList . PTPrelude.fromBuiltin $ bbs

-- | @since v4.0.0
instance Function ArbitraryTxId where
  function = functionMap into outOf
    where
      into :: ArbitraryTxId -> [Word8]
      into (ArbitraryTxId (TxId bbs)) = toList . PTPrelude.fromBuiltin $ bbs
      outOf :: [Word8] -> ArbitraryTxId
      outOf =
        ArbitraryTxId
          . TxId
          . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
          . fromList @ByteString

-- | Wrapper for 'TxOutRef' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryTxOutRef = ArbitraryTxOutRef TxOutRef
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    )
    via TxOutRef
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | @since v4.0.0
instance Arbitrary ArbitraryTxOutRef where
  arbitrary =
    ArbitraryTxOutRef <$> do
      NonNegative tidx <- arbitrary
      ArbitraryTxId tid <- arbitrary
      pure . TxOutRef tid $ tidx
  shrink (ArbitraryTxOutRef (TxOutRef tid tidx)) =
    ArbitraryTxOutRef <$> do
      NonNegative tidx' <- shrink . NonNegative $ tidx
      pure . TxOutRef tid $ tidx'

-- | @since v4.0.0
instance CoArbitrary ArbitraryTxOutRef where
  coarbitrary (ArbitraryTxOutRef (TxOutRef tid tidx)) =
    coarbitrary (ArbitraryTxId tid) . variant tidx

-- | @since v4.0.0
instance Function ArbitraryTxOutRef where
  function = functionMap into outOf
    where
      into :: ArbitraryTxOutRef -> (ArbitraryTxId, Integer)
      into (ArbitraryTxOutRef (TxOutRef tid tidx)) =
        (ArbitraryTxId tid, tidx)
      outOf :: (ArbitraryTxId, Integer) -> ArbitraryTxOutRef
      outOf (ArbitraryTxId tid, tidx) =
        ArbitraryTxOutRef . TxOutRef tid $ tidx

-- | Wrapper for 'LedgerBytes' to provide QuickCheck instances. This assumes any
-- kind of bytestring is OK.
--
-- @since v4.0.0
newtype ArbitraryBytes = ArbitraryBytes LedgerBytes
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via LedgerBytes
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | @since v4.0.0
instance Arbitrary ArbitraryBytes where
  arbitrary =
    ArbitraryBytes
      . LedgerBytes
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> arbitrary @[Word8]
  shrink (ArbitraryBytes (LedgerBytes bs)) = do
    shrunk <- shrink . toList . PTPrelude.fromBuiltin $ bs
    pure
      . ArbitraryBytes
      . LedgerBytes
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      $ shrunk

-- | @since v4.0.0
instance CoArbitrary ArbitraryBytes where
  coarbitrary (ArbitraryBytes (LedgerBytes bbs)) =
    coarbitrary . toList . PTPrelude.fromBuiltin $ bbs

-- | @since v4.0.0
instance Function ArbitraryBytes where
  function = functionMap into outOf
    where
      into :: ArbitraryBytes -> [Word8]
      into (ArbitraryBytes (LedgerBytes bs)) = toList . PTPrelude.fromBuiltin $ bs
      outOf :: [Word8] -> ArbitraryBytes
      outOf =
        ArbitraryBytes
          . LedgerBytes
          . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
          . fromList @ByteString

-- | Same as 'Test.QuickCheck.Gen.suchThat', but has a retry limit of 100; if it
-- fails to generate a satisfactory @a@ within that many attempts, the
-- generator will error out, and notify the user of this.
--
-- @since v3.0.0.
suchThat ::
  forall (a :: Type).
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThat = suchThatRetrying 100

-- | Same as 'Test.QuickCheck.Gen.suchThatMap', but has a retry limit of 100; if
-- it fails to generate a 'Just' within that many attempts, the generator will
-- error out, and notify the user of this.
--
-- @since v3.0.0.
suchThatMap ::
  forall (a :: Type) (b :: Type).
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMap = suchThatMapRetrying 100

-- | As 'suchThat', but allows setting the retry limit explicitly.
--
-- @since v3.0.0.
suchThatRetrying ::
  forall (a :: Type).
  Word ->
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThatRetrying limit gen p = sized (go 0)
  where
    go :: Word -> Int -> Gen a
    go !count !size =
      resize size gen >>= \x ->
        if
          | p x -> pure x
          | count == limit -> errorOut
          | otherwise -> go (count + 1) (size + 1)
    errorOut :: Gen a
    errorOut = error $ "suchThat exceeded retry limit: " <> show limit

-- | As 'suchThatMap', but allows setting the retry limit explicitly.
--
-- @since v3.0.0.
suchThatMapRetrying ::
  forall (a :: Type) (b :: Type).
  Word ->
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMapRetrying limit gen k = sized (go 0)
  where
    go :: Word -> Int -> Gen b
    go !count !size =
      resize size gen
        >>= ( k >>> \case
                Nothing ->
                  if count == limit
                    then errorOut
                    else go (count + 1) (size + 1)
                Just res -> pure res
            )
    errorOut :: Gen b
    errorOut = error $ "suchThatMap exceeded retry limit: " <> show limit

-- | As 'Test.QuickCheck.Gen.sublistOf', but about faster by a factor of 2-3.
--
-- @since v3.0.0.
sublistOf ::
  forall (a :: Type).
  [a] ->
  Gen [a]
sublistOf = \case
  [] -> pure []
  [x] -> elements [[], [x]]
  src -> arbitrary >>= go src (finiteBitSize @Word64 undefined)
  where
    go :: [a] -> Int -> Word64 -> Gen [a]
    go rest !bitsLeft !encoding = case rest of
      [] -> pure []
      (x : xs) ->
        let !shift = min bitsLeft (countTrailingZeros encoding)
         in if
              | shift > 0 -> go (drop shift rest) (bitsLeft - shift) (encoding `unsafeShiftR` shift)
              | bitsLeft == 0 -> arbitrary >>= go rest (finiteBitSize @Word64 undefined)
              | otherwise -> (x :) <$> go xs (bitsLeft - 1) (encoding `unsafeShiftR` 1)
