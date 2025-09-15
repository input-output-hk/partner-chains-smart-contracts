module Testing where

import Control.Lens
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusCore.Default (DefaultUni (DefaultUniUnit), Some (..), ValueOf (..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion100)
import PlutusTx
import PlutusTx.Prelude (BuiltinUnit)
import Test.Tasty
import Test.Tasty.HUnit
import UntypedPlutusCore.Core.Type (Term (Constant), progTerm)
import UntypedPlutusCore.Evaluation.Machine.Cek (counting, logEmitter)
import UntypedPlutusCore.Evaluation.Machine.Cek.Internal (runCekDeBruijn)

-- test functions

expectSuccess :: TestName -> CompiledCode BuiltinUnit -> TestTree
expectSuccess testName code = testCase testName do
  let plc = getPlc code ^. progTerm
  case runCekDeBruijn defaultCekParametersForTesting counting logEmitter plc of
    (Left ex, _counting, logs) -> assertFailure $ "Expected success but script failed! trace: " <> show logs <> "\n" <> show ex
    (Right actual, _counting, _logs) -> assertEqual "Evaluation has succeeded" constUnit actual
  where
    constUnit = Constant () (Some (ValueOf DefaultUniUnit ()))

expectFail :: TestName -> Text -> CompiledCode BuiltinUnit -> TestTree
expectFail testName expectedTrace code = testCase testName do
  let plc = getPlc code ^. progTerm
  case runCekDeBruijn defaultCekParametersForTesting counting logEmitter plc of
    (Left _ex, _counting, logs) ->
      assertBool
        ("Didn't fail with expected trace. expected: " <> Text.unpack expectedTrace <> " actual: " <> show logs)
        $ expectedTrace `elem` logs
    (Right _actual, _counting, _logs) -> assertFailure $ "Expected failure but script passed! expected: " <> Text.unpack expectedTrace

appArg :: (ToData b) => CompiledCode (BuiltinData -> a) -> b -> CompiledCode a
appArg a b = a `unsafeApplyCode` liftCode plcVersion100 (toBuiltinData b)
