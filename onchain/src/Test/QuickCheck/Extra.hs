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
import Data.Bits (testBit, unsafeShiftL, unsafeShiftR)
import Data.Kind (Type)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen qualified as Gen
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
sublistOf src = do
  let len = length src
  (\encoding -> go src encoding []) <$> Gen.chooseInteger (0, (1 `unsafeShiftL` len) - 1)
  where
    go :: [a] -> Integer -> [a] -> [a]
    go acc encoding = \case
      [] -> acc
      (x : xs) ->
        let !encoding' = encoding `unsafeShiftR` 1
         in if testBit encoding 0
              then go (x : acc) encoding' xs
              else go acc encoding' xs
