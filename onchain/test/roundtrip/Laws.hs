{-# LANGUAGE AllowAmbiguousTypes #-}

module Laws (
  toDataSafeLaws',
  toDataUnsafeLaws',
) where

import Data.Kind (Type)
import Data.String qualified as HString
import PlutusTx
import Test.QuickCheck (
  Gen,
  Property,
  counterexample,
  forAllShrinkShow,
  property,
 )

-- | As 'toDataSafeLaws', but allows a custom generator, shrinker and
-- prettyprinter.
toDataSafeLaws' ::
  forall (a :: Type).
  (ToData a, FromData a, Eq a) =>
  Gen a ->
  (a -> [a]) ->
  (a -> HString.String) ->
  Property
toDataSafeLaws' gen shr pprint = forAllShrinkShow gen shr pprint $ \x ->
  go (fromBuiltinData . toBuiltinData $ x) x
  where
    go :: Maybe a -> a -> Property
    go xs y = counterexample ("Original data: " <> pprint y) $
      case xs of
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
  (ToData a, UnsafeFromData a, Eq a) =>
  Gen a ->
  (a -> [a]) ->
  (a -> HString.String) ->
  Property
toDataUnsafeLaws' gen shr pprint = forAllShrinkShow gen shr pprint $ \x ->
  go (unsafeFromBuiltinData . toBuiltinData $ x) x
  where
    go :: a -> a -> Property
    go x1 x2 =
      counterexample ("Original data: " <> pprint x2) $
        if x1 == x2
          then property True
          else
            counterexample ("Decoded data did not match: " <> pprint x1)
              . property
              $ False
