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

import Acc (Acc)
import Acc qualified
import Control.Category ((>>>))
import Data.Bits (unsafeShiftL)
import Data.Kind (Type)
import Data.List.Split (chunksOf)
import Data.Word (Word64)
import GHC.Exts (toList)
import Test.QuickCheck (arbitrary)
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
  let !len = length src
  if
      | len < 64 -> do
        encoding <- Gen.chooseEnum (0, (1 `unsafeShiftL` len) - 1)
        pure . go encoding $ src
      | len == 64 -> (`go` src) <$> arbitrary
      | otherwise -> do
        let pieces = chunksOf 64 src
        let !tailCount = len `rem` 64
        combinedSublistOf tailCount pieces
  where
    go :: Word64 -> [a] -> [a]
    go encoding = case encoding `quotRem` 2 of
      (0, _) -> const []
      (encoding', 0) -> go encoding'
      (encoding', _) -> \case
        [] -> []
        (x : xs) -> x : go encoding' xs

-- Helpers

combinedSublistOf ::
  forall (a :: Type).
  Int ->
  [[a]] ->
  Gen [a]
combinedSublistOf tailLength srcs = toList <$> go srcs
  where
    go :: [[a]] -> Gen (Acc a)
    go = \case
      [] -> pure mempty
      [xs] -> do
        encoding <- Gen.chooseEnum (0, (1 `unsafeShiftL` tailLength) - 1)
        pure . goAcc encoding $ xs
      (xs : xss) -> (<>) <$> ((`goAcc` xs) <$> arbitrary) <*> go xss
    goAcc :: Word64 -> [a] -> Acc a
    goAcc encoding = case encoding `quotRem` 2 of
      (0, _) -> const mempty
      (encoding', 0) -> goAcc encoding'
      (encoding', _) -> \case
        [] -> mempty
        (x : xs) -> Acc.cons x . goAcc encoding' $ xs
