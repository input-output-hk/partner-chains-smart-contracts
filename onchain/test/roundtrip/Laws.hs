{-# LANGUAGE AllowAmbiguousTypes #-}

module Laws (
  toDataSafeLaws,
  toDataUnsafeLaws,
  toDataSafeLaws',
  toDataUnsafeLaws',
) where

import Data.String qualified as HString
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  Property,
  counterexample,
  forAllShrinkShow,
  property,
 )
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.PlutusPrelude qualified as PTPrelude

-- | Verifies that @'fromData' '.' 'toData'@ @=@ @'Just'@.
--
-- @since v4.0.0
toDataSafeLaws ::
  forall (a :: Type).
  ( Arbitrary a
  , PTPrelude.ToData a
  , PTPrelude.FromData a
  , Show a
  , Eq a
  ) =>
  Property
toDataSafeLaws = toDataSafeLaws' @a arbitrary shrink show

-- | Verified that @'unsafeFromData' '.' 'toData'@ @=@ @'id'@.
--
-- @since v4.0.0
toDataUnsafeLaws ::
  forall (a :: Type).
  ( Arbitrary a
  , PTPrelude.ToData a
  , PTPrelude.UnsafeFromData a
  , Show a
  , Eq a
  ) =>
  Property
toDataUnsafeLaws = toDataUnsafeLaws' @a arbitrary shrink show

-- | As 'toDataSafeLaws', but allows a custom generator, shrinker and
-- prettyprinter.
--
-- @since v4.0.0
toDataSafeLaws' ::
  forall (a :: Type).
  (PTPrelude.ToData a, PTPrelude.FromData a, Eq a) =>
  Gen a ->
  (a -> [a]) ->
  (a -> HString.String) ->
  Property
toDataSafeLaws' gen shr pprint = forAllShrinkShow gen shr pprint $ \x ->
  go (PTPrelude.fromBuiltinData . PTPrelude.toBuiltinData $ x) x
  where
    go :: Maybe a -> a -> Property
    go xs y = counterexample ("Original data: " <> pprint y)
      $ case xs of
        Nothing -> counterexample "Could not decode." . property $ False
        Just x1 ->
          if x1 == y
            then property True
            else
              counterexample ("Decoded data did not match: " <> pprint x1)
                . property
                $ False

-- | As 'toDataUnsafeLaws', but allows a custom generator, shrinker and
-- prettyprinter.
--
-- @since v4.0.0
toDataUnsafeLaws' ::
  forall (a :: Type).
  (PTPrelude.ToData a, PTPrelude.UnsafeFromData a, Eq a) =>
  Gen a ->
  (a -> [a]) ->
  (a -> HString.String) ->
  Property
toDataUnsafeLaws' gen shr pprint = forAllShrinkShow gen shr pprint $ \x ->
  go (PTPrelude.unsafeFromBuiltinData . PTPrelude.toBuiltinData $ x) x
  where
    go :: a -> a -> Property
    go x1 x2 =
      counterexample ("Original data: " <> pprint x2)
        $ if x1 == x2
          then property True
          else
            counterexample ("Decoded data did not match: " <> pprint x1)
              . property
              $ False
