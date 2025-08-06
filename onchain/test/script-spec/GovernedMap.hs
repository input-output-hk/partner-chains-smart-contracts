module GovernedMap where

import Control.Lens
import GHC.Exts (fromString)
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.GovernedMap
import TrustlessSidechain.ScriptId (governedMapPolicyId, governedMapValidatorId)
import TrustlessSidechain.Versioning (VersionOracleConfig)
import Prelude

-- | Top-level test group for both validator and policy
tests :: TestTree
tests =
  testGroup
    "GovernedMap scripts"
    [ policyTests
    , validatorTests
    ]

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
          & _scriptContextPurpose .~ V2.Minting Test.governedMapCurrSym
          -- signed by governance authority
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governedMapToken 1
          & _scriptContextTxInfo . _txInfoOutputs <>~ [governedMapTokenUtxo]
      )

governedMapPolicyFailing :: TestTree
governedMapPolicyFailing =
  expectFail "should fail" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.governedMapCurrSym
          -- not signed by governance authority
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governedMapToken 1
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
  expectFail "should fail" $
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
    Test.governedMapValidatorAddress
    (Test.governedMapToken 1)
    Test.dummyBuiltinData

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

runMintingPolicy :: V2.TxOutRef -> VersionOracleConfig -> BuiltinData -> V2.ScriptContext -> BuiltinUnit
runMintingPolicy genesisUtxo vc redeemer ctx =
  mkMintingPolicyUntyped
    (toBuiltinData governedMapPolicyId)
    (toBuiltinData genesisUtxo)
    (toBuiltinData vc)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)

runValidator :: V2.TxOutRef -> VersionOracleConfig -> BuiltinData -> BuiltinData -> V2.ScriptContext -> BuiltinUnit
runValidator genesisUtxo vc datum redeemer ctx =
  mkValidatorUntyped
    (toBuiltinData governedMapValidatorId)
    (toBuiltinData genesisUtxo)
    (toBuiltinData vc)
    (toBuiltinData datum)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)
