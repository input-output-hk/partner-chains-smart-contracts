{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sizer (fitsUnder) where

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
fitsUnder name test target = singleTest name $ SizeComparisonTest test target

-- Helpers

data SizeComparisonTest (a :: Type)
  = SizeComparisonTest
      (String, CompiledCode a)
      (String, CompiledCode a)

instance Typeable a => IsTest (SizeComparisonTest a) where
  run _ (SizeComparisonTest (mName, mCode) (tName, tCode)) _ = do
    let tEstimate = sizePlc tCode
    let mEstimate = sizePlc mCode
    let diff = tEstimate - mEstimate
    pure $ case signum diff of
      (-1) -> testFailed . renderFailed (tName, tEstimate) (mName, mEstimate) $ diff
      0 -> testPassed . renderEstimates (tName, tEstimate) $ (mName, mEstimate)
      _ -> testPassed . renderExcess (tName, tEstimate) (mName, mEstimate) $ diff
  testOptions = Tagged []

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
