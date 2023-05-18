{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Extra (
  suchThat,
  suchThatMap,
  suchThatRetrying,
  suchThatMapRetrying,
  sublistOf,
) where

import Control.Category ((>>>))
import Data.Kind (Type)
import Data.Word (Word64)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, elements)
import Prelude

suchThat ::
  forall (a :: Type).
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThat = suchThatRetrying 100

suchThatMap ::
  forall (a :: Type) (b :: Type).
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMap = suchThatMapRetrying 100

suchThatRetrying ::
  forall (a :: Type).
  Word ->
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThatRetrying limit gen p = go 0
  where
    go :: Word -> Gen a
    go !count =
      gen >>= \x ->
        if
            | p x -> pure x
            | count == limit -> errorOut
            | otherwise -> go (count + 1)
    errorOut :: Gen a
    errorOut = error $ "suchThat exceeded retry limit: " <> show limit

suchThatMapRetrying ::
  forall (a :: Type) (b :: Type).
  Word ->
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMapRetrying limit gen k = go 0
  where
    go :: Word -> Gen b
    go !count =
      gen
        >>= ( k >>> \case
                Nothing ->
                  if count == limit
                    then errorOut
                    else go (count + 1)
                Just res -> pure res
            )
    errorOut :: Gen b
    errorOut = error $ "suchThatMap exceeded retry limit: " <> show limit

sublistOf ::
  forall (a :: Type).
  [a] ->
  Gen [a]
sublistOf = \case
  [] -> pure []
  [x] -> elements [[], [x]]
  src -> arbitrary >>= go src 64
  where
    go :: [a] -> Int -> Word64 -> Gen [a]
    go rest bitsLeft encoding
      | bitsLeft == 0 = arbitrary >>= go rest 64
      | otherwise = case rest of
        [] -> pure []
        (x : xs) -> case encoding `quotRem` 2 of
          (encoding', 0) -> go xs (bitsLeft - 1) encoding'
          (encoding', _) -> (x :) <$> go xs (bitsLeft - 1) encoding'
