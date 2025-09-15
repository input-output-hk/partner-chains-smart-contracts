module Testing where

import Control.Exception (evaluate)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as BSS
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusCore.Default (DefaultUni (DefaultUniUnit), Some (..), ValueOf (..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Prelude (BuiltinUnit)
import Test.Tasty
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit
import UntypedPlutusCore.Core.Type (Term (Constant), progTerm)
import UntypedPlutusCore.Evaluation.Machine.Cek (CountingSt (..), counting, logEmitter)
import UntypedPlutusCore.Evaluation.Machine.Cek.Internal (runCekDeBruijn)

-- test functions

goldenSize :: String -> SerialisedScript -> TestTree
goldenSize name code = goldenVsVal cmp name ("./test/perf_data/" <> name <> "-size.json") $ getSize code
  where
    getSize = BSS.length
    cmp :: Int -> Int -> IO (Maybe String)
    cmp curr' new' = simpleCmp msg curr' new'
      where
        msg = "Script size " <> diffStr curr' new'

goldenPerf :: String -> CompiledCode BuiltinUnit -> TestTree
goldenPerf name code = goldenVsVal cmp name ("./test/perf_data/" <> name <> "-perf.json") $ getExecutionCost code
  where
    cmp :: V2.ExBudget -> V2.ExBudget -> IO (Maybe String)
    cmp
      curr'@V2.ExBudget {exBudgetCPU = V2.ExCPU currCpu, exBudgetMemory = V2.ExMemory currMem}
      new'@V2.ExBudget {exBudgetCPU = V2.ExCPU newCpu, exBudgetMemory = V2.ExMemory newMem} = simpleCmp msg curr' new'
        where
          msg = "Performance changed:\n  CPU " <> diffStr currCpu newCpu <> "\n  MEM " <> diffStr currMem newMem

simpleCmp :: (Eq a) => String -> a -> a -> IO (Maybe String)
simpleCmp e x y = return if x == y then Nothing else Just e

diffStr :: (Ord a, Show a, Num a) => a -> a -> String
diffStr curr new
  | curr < new = "INCREASED by " <> show (new - curr) <> " from " <> show curr <> " to " <> show new
  | curr > new = "DECREASED by " <> show (curr - new) <> " from " <> show curr <> " to " <> show new
  | otherwise = "did not change"

goldenVsVal ::
  (FromJSON a, ToJSON a) =>
  -- | comparison function
  (a -> a -> IO (Maybe String)) ->
  -- | test name
  TestName ->
  -- | path to the stats file that contains recorded value
  FilePath ->
  -- | current value
  a ->
  -- | the test verifies that the returned value is the same as the golden file contents
  TestTree
goldenVsVal cmp name ref exBudget =
  goldenTest
    name
    ((fromJust . decode) <$> readFileStrict ref)
    (pure exBudget)
    cmp
    (createDirectoriesAndWriteFile ref . (<> "\n") . encode)
  where
    readFileStrict :: FilePath -> IO LBS.ByteString
    readFileStrict path = do
      s <- LBS.readFile path
      evaluate $ forceLbs s
      return s

    -- Force the evaluation of a lazily-produced bytestring; This is important to close the file handles.
    -- See <https://ro-che.info/articles/2015-05-28-force-list>.
    forceLbs :: LBS.ByteString -> ()
    forceLbs = LBS.foldr seq ()

getExecutionCost :: CompiledCode BuiltinUnit -> V2.ExBudget
getExecutionCost code = do
  let plc = getPlc code ^. progTerm
  case runCekDeBruijn defaultCekParametersForTesting counting logEmitter plc of
    (Right _actual, CountingSt exBudget, _logs) -> exBudget
    (Left ex, _counting, logs) -> error $ "failed execution. trace: " <> show logs <> "\n" <> show ex

appArg :: (ToData b) => CompiledCode (BuiltinData -> a) -> b -> CompiledCode a
appArg a b = a `unsafeApplyCode` liftCode plcVersion100 (toBuiltinData b)

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
