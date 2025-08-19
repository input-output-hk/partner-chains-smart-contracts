{-# LANGUAGE AllowAmbiguousTypes #-}

module Laws (
  toDataSafeLaws',
  toDataUnsafeLaws',
) where

import Data.String qualified as HString
import Test.QuickCheck (
  Gen,
  Property,
  counterexample,
  forAllShrinkShow,
  property,
 )
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.PlutusPrelude qualified as PTPrelude

-- | As 'toDataSafeLaws', but allows a custom generator, shrinker and
-- prettyprinter.
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
