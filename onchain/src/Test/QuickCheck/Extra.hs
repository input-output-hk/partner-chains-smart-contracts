{-# LANGUAGE MultiWayIf #-}

{- | Module: Test.QuickCheck.Extra
 Description: Some improved versions and helpers for QuickCheck functions
 Copyright: (C) MLabs 2023
 Mainainter: Koz Ross (koz@mlabs.city)
 Portability: GHC only

 Some functions designed to supercede equivalent functionality from
 QuickCheck, for reasons of efficiency or safety.
-}
module Test.QuickCheck.Extra (
  -- * Generators
  suchThat,
  suchThatMap,
  suchThatRetrying,
  suchThatMapRetrying,
  sublistOf,
) where

import Data.Bits (
  countTrailingZeros,
  finiteBitSize,
  unsafeShiftR,
 )
import Data.List (drop)
import GHC.Err (undefined)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, elements, resize, sized)
import TrustlessSidechain.HaskellPrelude

{- | Same as 'Test.QuickCheck.Gen.suchThat', but has a retry limit of 100; if it
 fails to generate a satisfactory @a@ within that many attempts, the
 generator will error out, and notify the user of this.

 @since v3.0.0.
-}
suchThat ::
  forall (a :: Type).
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThat = suchThatRetrying 100

{- | Same as 'Test.QuickCheck.Gen.suchThatMap', but has a retry limit of 100; if
 it fails to generate a 'Just' within that many attempts, the generator will
 error out, and notify the user of this.

 @since v3.0.0.
-}
suchThatMap ::
  forall (a :: Type) (b :: Type).
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMap = suchThatMapRetrying 100

{- | As 'suchThat', but allows setting the retry limit explicitly.

 @since v3.0.0.
-}
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

{- | As 'suchThatMap', but allows setting the retry limit explicitly.

 @since v3.0.0.
-}
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

{- | As 'Test.QuickCheck.Gen.sublistOf', but about faster by a factor of 2-3.

 @since v3.0.0.
-}
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
