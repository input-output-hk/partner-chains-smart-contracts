module Sizer (
  fitsUnder,
  fitsInto,
  scriptFitsInto,
  scriptFitsUnder,
) where

import Data.String qualified as HString
import Data.Tagged (Tagged (Tagged))
import Plutus.V2.Ledger.Api (Script)
import PlutusTx.Code (CompiledCode, sizePlc)
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.ScriptUtils (scriptSize)
import Type.Reflection (Typeable)

fitsUnder ::
  forall (a :: Type).
  Typeable a =>
  HString.String ->
  (HString.String, CompiledCode a) ->
  (HString.String, CompiledCode a) ->
  TestTree
fitsUnder name test target = singleTest name $ SizeComparison test target

scriptFitsUnder ::
  HString.String ->
  (HString.String, Script) ->
  (HString.String, Script) ->
  TestTree
scriptFitsUnder name test target = singleTest name $ ScriptSizeComparison @() test target

fitsInto ::
  forall (a :: Type).
  Typeable a =>
  HString.String ->
  CompiledCode a ->
  Integer ->
  TestTree
fitsInto name code limit = singleTest name $ SizeBound code limit

scriptFitsInto ::
  HString.String ->
  Script ->
  Integer ->
  TestTree
scriptFitsInto name script limit = singleTest name $ ScriptSizeBound @() script limit

-- Helpers

data SizeTest (a :: Type)
  = SizeBound (CompiledCode a) Integer
  | SizeComparison (HString.String, CompiledCode a) (HString.String, CompiledCode a)
  | ScriptSizeBound Script Integer
  | ScriptSizeComparison (HString.String, Script) (HString.String, Script)

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
    ScriptSizeBound script limit -> do
      let estimate = scriptSize script
      let diff = limit - estimate
      pure $ case signum diff of
        (-1) -> testFailed $ "Exceeded limit by " <> show (abs diff)
        0 -> testPassed $ "Size: " <> show estimate
        _ -> testPassed $ "Remaining headroom: " <> show diff
    ScriptSizeComparison (mName, mScript) (tName, tScript) -> do
      let tEstimate = scriptSize tScript
      let mEstimate = scriptSize mScript
      let diff = tEstimate - mEstimate
      pure $ case signum diff of
        (-1) -> testFailed . renderFailed (tName, tEstimate) (mName, mEstimate) $ diff
        0 -> testPassed . renderEstimates (tName, tEstimate) $ (mName, mEstimate)
        _ -> testPassed . renderExcess (tName, tEstimate) (mName, mEstimate) $ diff

renderFailed ::
  (HString.String, Integer) ->
  (HString.String, Integer) ->
  Integer ->
  HString.String
renderFailed tData mData diff =
  renderEstimates tData mData
    <> "Exceeded by: "
    <> show (abs diff)

renderEstimates ::
  (HString.String, Integer) ->
  (HString.String, Integer) ->
  HString.String
renderEstimates (tName, tEstimate) (mName, mEstimate) =
  "Target: "
    <> tName
    <> "; size "
    <> show tEstimate
    <> "\n"
    <> "Measured: "
    <> mName
    <> "; size "
    <> show mEstimate
    <> "\n"

renderExcess ::
  (HString.String, Integer) ->
  (HString.String, Integer) ->
  Integer ->
  HString.String
renderExcess tData mData diff =
  renderEstimates tData mData
    <> "Remaining headroom: "
    <> show diff
