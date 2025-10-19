module Testing where

import Prelude

import Control.Exception (evaluate)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as BSS
import Data.Maybe (fromJust)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (MajorProtocolVersion (..), PlutusLedgerLanguage (..), ScriptNamedDeBruijn (..), SerialisedScript, deserialiseScript, deserialisedScript)
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Prelude (BuiltinUnit)
import Test.Tasty
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import Test.Tasty.Golden.Advanced (goldenTest)
import UntypedPlutusCore.Core.Type (progTerm)
import UntypedPlutusCore.Evaluation.Machine.Cek (CountingSt (..), counting, logEmitter)
import UntypedPlutusCore.Evaluation.Machine.Cek.Internal (runCekDeBruijn)

-- test functions

goldenSize :: String -> SerialisedScript -> TestTree
goldenSize name code = goldenVsVal cmp name ("./test/size_data/" <> name <> ".json") $ getSize code
  where
    getSize = BSS.length
    cmp :: Int -> Int -> IO (Maybe String)
    cmp curr' new' = simpleCmp msg curr' new'
      where
        msg = "Script size " <> diffStr curr' new'

goldenPerfV3 :: String -> SerialisedScript -> TestTree
goldenPerfV3 name code = goldenVsVal cmp name ("./test/perf_data/" <> name <> "_v3.json") $ getExecutionCostV3 code
  where
    cmp :: V2.ExBudget -> V2.ExBudget -> IO (Maybe String)
    cmp
      curr'@V2.ExBudget {exBudgetCPU = V2.ExCPU currCpu, exBudgetMemory = V2.ExMemory currMem}
      new'@V2.ExBudget {exBudgetCPU = V2.ExCPU newCpu, exBudgetMemory = V2.ExMemory newMem} = simpleCmp msg curr' new'
        where
          msg = "Performance changed:\n  CPU " <> diffStr currCpu newCpu <> "\n  MEM " <> diffStr currMem newMem

goldenPerf :: String -> CompiledCode BuiltinUnit -> TestTree
goldenPerf name code = goldenVsVal cmp name ("./test/perf_data/" <> name <> ".json") $ getExecutionCost code
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

getExecutionCostV3 :: SerialisedScript -> V2.ExBudget
getExecutionCostV3 serialised = do
  let jazda = case deserialiseScript PlutusV3 (MajorProtocolVersion 8) serialised of
        Right x -> x
        Left _ -> error "dupa"
  let (ScriptNamedDeBruijn prog) = deserialisedScript jazda
  let plc = prog ^. progTerm
  case runCekDeBruijn defaultCekParametersForTesting counting logEmitter plc of
    (Right _actual, CountingSt exBudget, _logs) -> exBudget
    (Left ex, _counting, logs) -> error $ "failed execution. trace: " <> show logs <> "\n" <> show ex

appArg :: (ToData b) => CompiledCode (BuiltinData -> a) -> b -> CompiledCode a
appArg a b =
  case a `applyCode` liftCode plcVersion100 (toBuiltinData b) of
    Right code -> code
    Left err -> error err
