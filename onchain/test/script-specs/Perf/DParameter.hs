module Perf.DParameter (execCosts) where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.DParameter
import TrustlessSidechain.Types qualified as Types

execCosts :: TestTree
execCosts =
  testGroup
    "DParameter scripts ExBudget"
    [ goldenPerf "dParamMintingPolicy" dParamMintingPolicy
    , goldenPerf "dParamValidatorNotSpending" dParamValidatorNotSpending
    , goldenPerf "dParamValidatorSpending" dParamValidatorSpending
    ]

dParamMintingPolicy :: CompiledCode BuiltinUnit
dParamMintingPolicy =
  runMintingPolicy
    Test.genesisUtxo
    Test.versionOracleConfig
    dParameterValidatorAddress
    Test.dummyBuiltinData
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Minting dParameterCurrSym
        -- signed by governance:
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
        -- minted tokens are all sent to dParameterValidatorAddress:
        & _scriptContextTxInfo . _txInfoMint <>~ dParameterOracleToken 2
        & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
        & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
    )

dParamValidatorNotSpending :: CompiledCode BuiltinUnit
dParamValidatorNotSpending =
  runValidator
    Test.genesisUtxo
    Test.versionOracleConfig
    Test.dummyBuiltinData
    Test.dummyBuiltinData
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Spending someTxOutRef
        -- signed by governance:
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
    )

dParamValidatorSpending :: CompiledCode BuiltinUnit
dParamValidatorSpending =
  runValidator
    Test.genesisUtxo
    Test.versionOracleConfig
    Test.dummyBuiltinData
    Test.dummyBuiltinData
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Spending someTxOutRef
        -- signed by governance:
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
        -- we spend a d-param utxo here:
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ dParamTokenUtxo]
    )

-- values

dParamTokenUtxo :: V2.TxOut
dParamTokenUtxo =
  mkTxOut
    dParameterValidatorAddress
    (dParameterOracleToken 1)
    Test.dummyBuiltinData
    dParameterValidatorScriptHash

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

dParameterValidatorAddress :: V2.Address
dParameterValidatorAddress = V2.Address (V2.PubKeyCredential "45674567456745674567456745674567456745674567456745674567") Nothing

dParameterCurrSym :: V2.CurrencySymbol
dParameterCurrSym = V2.CurrencySymbol . V2.getScriptHash $ dParameterValidatorScriptHash

dParameterValidatorScriptHash :: V2.ScriptHash
dParameterValidatorScriptHash = V2.ScriptHash "dParameterValidatorScriptHash"

dParameterOracleToken :: Integer -> V2.Value
dParameterOracleToken = V2.singleton dParameterCurrSym dParameterOracleTokenName
  where
    dParameterOracleTokenName :: V2.TokenName
    dParameterOracleTokenName = V2.TokenName "dParameter oracle"

-- test runner

runMintingPolicy :: V2.TxOutRef -> Types.VersionOracleConfig -> V2.Address -> BuiltinData -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy genesisUtxo vc dParameterValidatorAddress' redeemer ctx =
  compiledMintingPolicy
    `appArg` genesisUtxo
    `appArg` vc
    `appArg` dParameterValidatorAddress'
    `appArg` redeemer
    `appArg` ctx

runValidator :: V2.TxOutRef -> Types.VersionOracleConfig -> BuiltinData -> BuiltinData -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator genesisUtxo vc datum redeemer ctx =
  compiledValidator
    `appArg` genesisUtxo
    `appArg` vc
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
