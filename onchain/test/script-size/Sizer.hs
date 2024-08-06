module Sizer (
  scriptFitsInto,
  scriptFitsUnder,
) where

import Data.ByteString.Short qualified
import Data.String qualified as HString
import Data.Tagged (Tagged (Tagged))
import GHC.Num (integerFromInt)
import PlutusLedgerApi.Common (SerialisedScript)
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import TrustlessSidechain.HaskellPrelude
import Type.Reflection (Typeable)

scriptFitsUnder ::
  HString.String ->
  (HString.String, SerialisedScript) ->
  (HString.String, SerialisedScript) ->
  TestTree
scriptFitsUnder name test target = singleTest name $ ScriptSizeComparison @() test target

scriptFitsInto ::
  HString.String ->
  SerialisedScript ->
  Integer ->
  TestTree
scriptFitsInto name script limit =
  singleTest name $ ScriptSizeBound @() script limit

-- Helpers

data SizeTest (a :: Type)
  = ScriptSizeBound SerialisedScript Integer
  | ScriptSizeComparison (HString.String, SerialisedScript) (HString.String, SerialisedScript)

instance (Typeable a) => IsTest (SizeTest a) where
  testOptions = Tagged []
  run _ testData _ = case testData of
    ScriptSizeBound script limit -> do
      let estimate = scriptSize script
      let diff = limit - estimate
      pure $ case signum diff of
        -1 ->
          testFailed
            $ "Known script size INCREASED by "
            <> show (abs diff)
            <> " (New size: "
            <> show estimate
            <> ")"
            <> " Please make sure this is intentional!"
        0 -> testPassed $ "Size: " <> show estimate
        _ ->
          testFailed
            $ "Known script size decreased by "
            <> show diff
            <> " (New size: "
            <> show estimate
            <> ")"
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

-- Attempt to estimate the script size.  Note that:
--
--  * `programSize` counts the number of AST nodes in a program, which is very
--     much different from the actual size of serialized script added to the
--     transaction.
--
--  * We optimize the script before estiamting its size, because this gets us
--    closer to how the scripts are being serialized for the off-chain code.
--
--  * Plutus abandoned CBOR serialization in favour of flat serialization. See
--    https://github.com/input-output-hk/plutus/blob/master/doc/notes/plutus-core/Serialisation-flat.md
--    for details.
--
--  * In order for any size approximation to make sense it has to be monotone.
--    In other words, if we attempt to approximate the size of compiled scripts
--    by AST size it must be the case that:
--
--       size(AST_1) < size(AST_2)  ==>  size(flat(AST_1)) <= size(flat(AST_2))
--
--    where "AST" is a UPLC program's AST, "size" computes the size, and "flat"
--    performs flat encoding, or any other encoding used by cardano-node to
--    represent transactions.  Note that we don't require strict monotonicity.
--    If monotonicity doesn't hold then our size estimations will be useless,
--    because then it can happen that making the AST size smaller will make the
--    flat encoding larger.
scriptSize :: SerialisedScript -> Integer
scriptSize = integerFromInt . Data.ByteString.Short.length
