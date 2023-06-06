{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sizer (fitsUnder, fitsInto) where

import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import PlutusTx.Code (CompiledCode, sizePlc)
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import Type.Reflection (Typeable)
import Prelude

fitsUnder ::
  forall (a :: Type).
  Typeable a =>
  String ->
  (String, CompiledCode a) ->
  (String, CompiledCode a) ->
  TestTree
fitsUnder name test target = singleTest name $ SizeComparison test target

fitsInto ::
  forall (a :: Type).
  Typeable a =>
  String ->
  CompiledCode a ->
  Integer ->
  TestTree
fitsInto name code limit = singleTest name $ SizeBound code limit

-- Helpers

data SizeTest (a :: Type)
  = SizeBound (CompiledCode a) Integer
  | SizeComparison (String, CompiledCode a) (String, CompiledCode a)

instance Typeable a => IsTest (SizeTest a) where
  testOptions = Tagged []
  run _ testData _ = case testData of
    SizeBound code limit -> do
      let estimate = sizePlc code
      let diff = limit - estimate
      pure $ case signum diff of
        (-1) -> testFailed $ "Exceeded limit by " <> show (abs diff)
        0 -> testPassed $ "Size: " <> show estimate
        _ -> testPassed $ "Remaining headroom: " <> show diff
    SizeComparison (mName, mCode) (tName, tCode) -> do
      let tEstimate = sizePlc tCode
      let mEstimate = sizePlc mCode
      let diff = tEstimate - mEstimate
      pure $ case signum diff of
        (-1) -> testFailed . renderFailed (tName, tEstimate) (mName, mEstimate) $ diff
        0 -> testPassed . renderEstimates (tName, tEstimate) $ (mName, mEstimate)
        _ -> testPassed . renderExcess (tName, tEstimate) (mName, mEstimate) $ diff

renderFailed ::
  (String, Integer) ->
  (String, Integer) ->
  Integer ->
  String
renderFailed tData mData diff =
  renderEstimates tData mData
    <> "Exceeded by: "
    <> show diff

renderEstimates ::
  (String, Integer) ->
  (String, Integer) ->
  String
renderEstimates (tName, tEstimate) (mName, mEstimate) =
  "Target: " <> tName <> "; size " <> show tEstimate <> "\n"
    <> "Measured: "
    <> mName
    <> "; size "
    <> show mEstimate
    <> "\n"

renderExcess ::
  (String, Integer) ->
  (String, Integer) ->
  Integer ->
  String
renderExcess tData mData diff =
  renderEstimates tData mData
    <> "Remaining headroom: "
    <> show diff
