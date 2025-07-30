module DParameter where

import Control.Lens
import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.DParameter
import TrustlessSidechain.Types
import Prelude

-- minting policy

policyTests :: TestTree
policyTests =
  testGroup
    "d-parameter policy"
    [ dParamMintingPolicyPassing
    , dParamMintingPolicyFailing01
    , dParamMintingPolicyFailing02
    , dParamMintingPolicyFailing03
    ]

dParamMintingPolicyPassing :: TestTree
dParamMintingPolicyPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dParameterValidatorAddress
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.dParameterCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- minted tokens are all sent to dParameterValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ Test.dParameterOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
      )

dParamMintingPolicyFailing01 :: TestTree
dParamMintingPolicyFailing01 =
  expectFail "should fail if not signed by the governance authority (ERROR-DPARAMETER-POLICY-01)" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dParameterValidatorAddress
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.dParameterCurrSym
          -- not signed by governance (token missing):
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- minted tokens are all sent to dParameterValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ Test.dParameterOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
      )

dParamMintingPolicyFailing02 :: TestTree
dParamMintingPolicyFailing02 =
  expectFail "should fail if some tokens are not sent to dParameterValidatorAddress (ERROR-DPARAMETER-POLICY-02)" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dParameterValidatorAddress
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.dParameterCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- one minted token is sent to the wrong address:
          & _scriptContextTxInfo . _txInfoMint <>~ Test.dParameterOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [wrongAddressTokenUtxo]
      )
  where
    wrongValidatorAddress = V2.Address (V2.PubKeyCredential "01230123012301230123012301230123012301230123012301230123") Nothing

    wrongAddressTokenUtxo =
      mkTxOut
        wrongValidatorAddress
        (Test.dParameterOracleToken 1)
        Test.dummyBuiltinData
        Test.dParameterValidatorScriptHash

dParamTokenUtxo :: V2.TxOut
dParamTokenUtxo =
  mkTxOut
    Test.dParameterValidatorAddress
    (Test.dParameterOracleToken 1)
    Test.dummyBuiltinData
    Test.dParameterValidatorScriptHash

dParamMintingPolicyFailing03 :: TestTree
dParamMintingPolicyFailing03 =
  expectFail "should fail if script purpose is not Minting (ERROR-DPARAMETER-POLICY-03)" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dParameterValidatorAddress
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending (V2.TxOutRef "some utxo" 0)
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- minted tokens are all sent to dParameterValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ Test.dParameterOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [dParamTokenUtxo]
      )

-- validator

validatorTests :: TestTree
validatorTests =
  testGroup
    "d-parameter validator"
    [ dParamValidatorPassing
    , dParamValidatorFailing01
    ]

dParamValidatorPassing :: TestTree
dParamValidatorPassing =
  expectSuccess "should pass" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.dParameterCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

dParamValidatorFailing01 :: TestTree
dParamValidatorFailing01 =
  expectFail "should fail if not signed by the governance authority (ERROR-DPARAMETER-VALIDATOR-01)" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.dParameterCurrSym
          -- not signed by governance (token missing):
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
      )

-- test runner

runMintingPolicy :: V2.TxOutRef -> VersionOracleConfig -> V2.Address -> BuiltinData -> V2.ScriptContext -> BuiltinUnit
runMintingPolicy genesisUtxo vc dParameterValidatorAddress redeemer ctx =
  mkMintingPolicyUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData vc)
    (toBuiltinData dParameterValidatorAddress)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)

runValidator :: V2.TxOutRef -> VersionOracleConfig -> BuiltinData -> BuiltinData -> V2.ScriptContext -> BuiltinUnit
runValidator genesisUtxo vc datum redeemer ctx =
  mkValidatorUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData vc)
    (toBuiltinData datum)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)
