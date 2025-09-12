{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ScriptPerfUtils where

import Control.Exception (evaluate)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (BuiltinUnit)
import Test.Tasty
import Test.Tasty.Golden (createDirectoriesAndWriteFile)
import Test.Tasty.Golden.Advanced (goldenTest)
import UntypedPlutusCore.Core.Type (progTerm)
import UntypedPlutusCore.Evaluation.Machine.Cek (CountingSt (..), counting, logEmitter)
import UntypedPlutusCore.Evaluation.Machine.Cek.Internal (runCekDeBruijn)

emptyScriptContext :: V2.ScriptContext
emptyScriptContext =
  V2.ScriptContext
    { V2.scriptContextTxInfo = emptyTxInfo
    , V2.scriptContextPurpose = undefined
    }

emptyTxInfo :: V2.TxInfo
emptyTxInfo =
  V2.TxInfo
    { V2.txInfoInputs = []
    , V2.txInfoReferenceInputs = []
    , V2.txInfoOutputs = []
    , V2.txInfoFee = emptyValue
    , V2.txInfoMint = emptyValue
    , V2.txInfoDCert = []
    , V2.txInfoWdrl = Map.empty
    , V2.txInfoValidRange = interval (V2.POSIXTime 0) (V2.POSIXTime 0)
    , V2.txInfoSignatories = []
    , V2.txInfoRedeemers = Map.empty
    , V2.txInfoData = Map.empty
    , V2.txInfoId = V2.TxId ""
    }

emptyTxInInfo :: V2.TxInInfo
emptyTxInInfo =
  V2.TxInInfo
    { V2.txInInfoOutRef = emptyTxOutRef
    , V2.txInInfoResolved = emptyTxOut
    }

emptyTxOutRef :: V2.TxOutRef
emptyTxOutRef = V2.TxOutRef "" 0

emptyTxOut :: V2.TxOut
emptyTxOut =
  V2.TxOut
    { V2.txOutAddress = V2.Address (V2.PubKeyCredential "") Nothing
    , V2.txOutValue = emptyValue
    , V2.txOutDatum = V2.NoOutputDatum
    , V2.txOutReferenceScript = Nothing
    }

emptyValue :: V2.Value
emptyValue = V2.Value Map.empty

mkTxOut :: (ToData d) => V2.Address -> V2.Value -> d -> V2.ScriptHash -> V2.TxOut
mkTxOut txOutAddress txOutValue datum scriptHash =
  V2.TxOut
    { V2.txOutAddress = txOutAddress
    , V2.txOutValue = txOutValue
    , V2.txOutDatum = V2.OutputDatum (V2.Datum $ toBuiltinData datum)
    , V2.txOutReferenceScript = Just scriptHash
    }

mkTxOutNoScriptHash :: (ToData d) => V2.Address -> V2.Value -> d -> V2.TxOut
mkTxOutNoScriptHash txOutAddress txOutValue datum =
  V2.TxOut
    { V2.txOutAddress = txOutAddress
    , V2.txOutValue = txOutValue
    , V2.txOutDatum = V2.OutputDatum (V2.Datum $ toBuiltinData datum)
    , V2.txOutReferenceScript = Nothing
    }

-- test functions

goldenPerf :: String -> CompiledCode BuiltinUnit -> TestTree
goldenPerf name code = goldenVsVal cmp name ("./test/perf_data/" <> name <> "-perf.json") $ getExecutionCost code
  where
    cmp :: V2.ExBudget -> V2.ExBudget -> IO (Maybe String)
    cmp
      curr'@V2.ExBudget {exBudgetCPU = V2.ExCPU currCpu, exBudgetMemory = V2.ExMemory currMem}
      new'@V2.ExBudget {exBudgetCPU = V2.ExCPU newCpu, exBudgetMemory = V2.ExMemory newMem} = simpleCmp msg curr' new'
        where
          msg = "Performance changed:\n  CPU " <> diffStr currCpu newCpu <> "\n  MEM " <> diffStr currMem newMem
          diffStr curr new
            | curr < new = "INCREASED by " <> show (new - curr) <> " from " <> show curr <> " to " <> show new
            | curr > new = "DECREASED by " <> show (curr - new) <> " from " <> show curr <> " to " <> show new
            | otherwise = "did not change"

simpleCmp :: (Eq a) => String -> a -> a -> IO (Maybe String)
simpleCmp e x y = return if x == y then Nothing else Just e

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

-- lens

{-

Lens allow for more readable construction of `ScriptContext` objects.
Lens are composable functions that can be used for defining a path through the fields of a compound object.
This path can then be used for both setting and getting values in an object.

To construct `ScriptContext`s for tests, one can start with an `emptyScriptContext` and set its fields with lens as needed.
For example:

emptyScriptContext & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken

The (&) operator is the flipped version of ($).
Lens can be composed like regular functions (which is what they are).
Then one can set them with different operators:
- (.~) sets the value
- (<>~) appends to the value
- (%~) applies a function to the value

Multiple of these setters can be chained as well. For example:

emptyScriptContext
    & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
    & _scriptContextTxInfo . _txInfoInputs %~ take 1
    & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym

We prefix lens functions with underscore to make them easier to distinguish from regular functions.

-}

_scriptContextTxInfo :: Lens' V2.ScriptContext V2.TxInfo
_scriptContextTxInfo = lens V2.scriptContextTxInfo \a scriptContextTxInfo -> a {V2.scriptContextTxInfo = scriptContextTxInfo}

_scriptContextPurpose :: Lens' V2.ScriptContext V2.ScriptPurpose
_scriptContextPurpose = lens V2.scriptContextPurpose \a scriptContextPurpose -> a {V2.scriptContextPurpose = scriptContextPurpose}

_txInfoInputs :: Lens' V2.TxInfo [V2.TxInInfo]
_txInfoInputs = lens V2.txInfoInputs \a txInfoInputs -> a {V2.txInfoInputs = txInfoInputs}

_txInfoReferenceInputs :: Lens' V2.TxInfo [V2.TxInInfo]
_txInfoReferenceInputs = lens V2.txInfoReferenceInputs \a txInfoReferenceInputs -> a {V2.txInfoReferenceInputs = txInfoReferenceInputs}

_txInfoOutputs :: Lens' V2.TxInfo [V2.TxOut]
_txInfoOutputs = lens V2.txInfoOutputs \a txInfoOutputs -> a {V2.txInfoOutputs = txInfoOutputs}

_txInfoMint :: Lens' V2.TxInfo V2.Value
_txInfoMint = lens V2.txInfoMint \a txInfoMint -> a {V2.txInfoMint = txInfoMint}

_txInInfoOutRef :: Lens' V2.TxInInfo V2.TxOutRef
_txInInfoOutRef = lens V2.txInInfoOutRef \a txInInfoOutRef -> a {V2.txInInfoOutRef = txInInfoOutRef}

_txInInfoResolved :: Lens' V2.TxInInfo V2.TxOut
_txInInfoResolved = lens V2.txInInfoResolved \a txInInfoResolved -> a {V2.txInInfoResolved = txInInfoResolved}

_txInfoSignatories :: Lens' V2.TxInfo [V2.PubKeyHash]
_txInfoSignatories = lens V2.txInfoSignatories \a txInfoSignatories -> a {V2.txInfoSignatories = txInfoSignatories}

_txOutDatum :: Lens' V2.TxOut V2.OutputDatum
_txOutDatum = lens V2.txOutDatum \a txOutDatum -> a {V2.txOutDatum = txOutDatum}

_txOutAddress :: Lens' V2.TxOut V2.Address
_txOutAddress = lens V2.txOutAddress \a txOutAddress -> a {V2.txOutAddress = txOutAddress}

_txOutValue :: Lens' V2.TxOut V2.Value
_txOutValue = lens V2.txOutValue \a txOutValue -> a {V2.txOutValue = txOutValue}
