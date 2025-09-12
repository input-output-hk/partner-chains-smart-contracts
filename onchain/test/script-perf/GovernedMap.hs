module GovernedMap (execCosts) where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptPerfUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.GovernedMap (compiledMintingPolicy, compiledValidator)
import TrustlessSidechain.ScriptId (governedMapPolicyId, governedMapValidatorId)
import TrustlessSidechain.Types qualified as Types

execCosts :: TestTree
execCosts =
  testGroup
    "GovernedMap scripts ExBudget"
    [ goldenPerf "governedMapPolicy" governedMapPolicy
    , goldenPerf "governedMapValidator" governedMapValidator
    ]

governedMapPolicy :: CompiledCode BuiltinUnit
governedMapPolicy =
  runMintingPolicy
    Test.genesisUtxo
    Test.versionOracleConfig
    Test.dummyBuiltinData
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Minting governedMapCurrSym
        -- signed by governance authority
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
        & _scriptContextTxInfo . _txInfoMint <>~ governedMapToken 1
        & _scriptContextTxInfo . _txInfoOutputs <>~ [governedMapTokenUtxo]
    )

governedMapValidator :: CompiledCode BuiltinUnit
governedMapValidator =
  runValidator
    Test.genesisUtxo
    Test.versionOracleConfig
    Test.dummyBuiltinData
    Test.dummyBuiltinData
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Spending someTxOutRef
        -- signed by governance
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governedMapTokenUtxo]
    )

governedMapTokenUtxo :: V2.TxOut
governedMapTokenUtxo =
  mkTxOutNoScriptHash
    governedMapValidatorAddress
    (governedMapToken 1)
    Test.dummyBuiltinData

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

governedMapCurrSym :: V2.CurrencySymbol
governedMapCurrSym = V2.CurrencySymbol "governedMapCurrSym"

governedMapTokenName :: V2.TokenName
governedMapTokenName = V2.TokenName "Governed map"

governedMapToken :: Integer -> V2.Value
governedMapToken = V2.singleton governedMapCurrSym governedMapTokenName

governedMapValidatorAddress :: V2.Address
governedMapValidatorAddress = V2.Address (V2.PubKeyCredential "01230123012301230123012301230123012301230123012301230123") Nothing

-- test runner

runMintingPolicy :: V2.TxOutRef -> Types.VersionOracleConfig -> BuiltinData -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy genesisUtxo vc redeemer ctx =
  compiledMintingPolicy
    `appArg` governedMapPolicyId
    `appArg` genesisUtxo
    `appArg` vc
    `appArg` redeemer
    `appArg` ctx

runValidator :: V2.TxOutRef -> Types.VersionOracleConfig -> BuiltinData -> BuiltinData -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator genesisUtxo vc datum redeemer ctx =
  compiledValidator
    `appArg` governedMapValidatorId
    `appArg` genesisUtxo
    `appArg` vc
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
