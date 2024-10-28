module Test.InitSidechain.Utils
  ( failMsg
  , unorderedEq
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map

-- | Testing utility to check ordered equality of
-- | Map.Map, whose Eq instance is derived from the Array Eq instance
-- | and therefore is sensitive to the order of insertion.
-- | Note this is not *set* equality, since there is no deduplication.
unorderedEq ::
  forall k v.
  Ord k =>
  Ord v =>
  Map.Map k v ->
  Map.Map k v ->
  Boolean
unorderedEq m1 m2 =
  let
    kvs m = Array.sort $ Map.toUnfoldable m
  in
    kvs m1 == kvs m2

-- | Testing utility for showing expected/actual
failMsg :: forall a b. Show a => Show b => a -> b -> String
failMsg exp act = "Expected: "
  <> show exp
  <> "\nBut got: "
  <> show act