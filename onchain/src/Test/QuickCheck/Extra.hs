{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Category ((>>>))
import Data.Bits (
  countTrailingZeros,
  finiteBitSize,
  unsafeShiftR,
 )
import Data.Kind (Type)
import Data.Word (Word64)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, elements, resize, sized)
import Prelude

{- | Same as 'Test.QuickCheck.Gen.suchThat', but has a retry limit of 100; if it
 fails to generate a satisfactory @a@ within that many attempts, the
 generator will error out, and notify the user of this.

 @since Unreleased
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

 @since Unreleased
-}
suchThatMap ::
  forall (a :: Type) (b :: Type).
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMap = suchThatMapRetrying 100

{- | As 'suchThat', but allows setting the retry limit explicitly.

 @since Unreleased
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
    go !count size =
      resize size gen >>= \x ->
        if
            | p x -> pure x
            | count == limit -> errorOut
            | otherwise -> go (count + 1) (size + 1)
    errorOut :: Gen a
    errorOut = error $ "suchThat exceeded retry limit: " <> show limit

{- | As 'suchThatMap', but allows setting the retry limit explicitly.

 @since Unreleased
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
    go !count size =
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

{- | As 'Test.QuickCheck.Gen.sublistOf', but about twice as fast.

 @since Unreleased
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
      whole@(x : xs) ->
        if bitsLeft == 0
          then arbitrary >>= go rest (finiteBitSize @Word64 undefined)
          else
            let !counted = countTrailingZeros encoding
             in if counted == 0
                  then (x :) <$> go xs (bitsLeft - 1) (encoding `unsafeShiftR` 1)
                  else
                    let !shift = min bitsLeft counted
                     in go (drop shift whole) (bitsLeft - shift) (encoding `unsafeShiftR` shift)
