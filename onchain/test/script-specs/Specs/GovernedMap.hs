module Specs.GovernedMap where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.GovernedMap
import TrustlessSidechain.ScriptId (governedMapPolicyId, governedMapValidatorId)
import TrustlessSidechain.Types qualified as Types

validatorTests :: TestTree
validatorTests =
  testGroup
    "governed-map validator"
    [ governedMapValidatorPassing
    , governedMapValidatorFailing
    ]

policyTests :: TestTree
policyTests =
  testGroup
    "governed-map policy"
    [ governedMapPolicyPassing
    , governedMapPolicyFailing
    ]

governedMapPolicyPassing :: TestTree
governedMapPolicyPassing =
  expectSuccess "should pass" $
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

governedMapPolicyFailing :: TestTree
governedMapPolicyFailing =
  expectFail "should fail if not signed by the governance authority" "ERROR-GOVERNED-MAP-POLICY-01" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting governedMapCurrSym
          -- not signed by governance authority
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ governedMapToken 1
          & _scriptContextTxInfo . _txInfoOutputs <>~ [governedMapTokenUtxo]
      )

governedMapValidatorPassing :: TestTree
governedMapValidatorPassing =
  expectSuccess "should pass" $
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

governedMapValidatorFailing :: TestTree
governedMapValidatorFailing =
  expectFail "should fail if not signed by the governance authority" "ERROR-GOVERNED-MAP-VALIDATOR-01" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          -- not signed by governance
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
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
