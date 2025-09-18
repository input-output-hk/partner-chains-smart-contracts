module Specs.PermissionedCandidates (
  policyTests,
  validatorTests,
) where

import ApiBuilder
import Control.Lens
import PartnerChains.Scripts.PermissionedCandidates
import PartnerChains.Types qualified as Types
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import Test.Tasty
import TestValues qualified as Test
import Testing

-- minting policy

policyTests :: TestTree
policyTests =
  testGroup
    "permissioned candidates policy"
    [ testGroup
        "mint redeemer"
        [ permissionedCandidatesPolicyMintPassing
        , permissionedCandidatesPolicyMintFailing01
        , permissionedCandidatesPolicyMintFailing02
        ]
    , testGroup
        "burn redeemer"
        [ permissionedCandidatesPolicyBurnPassing
        , permissionedCandidatesPolicyBurnFailing03
        , permissionedCandidatesPolicyBurnFailing04
        ]
    , permissionedCandidatesPolicyBurnFailing05
    ]

permissionedCandidatesPolicyMintPassing :: TestTree
permissionedCandidatesPolicyMintPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesMint
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting permissionedCandidatesCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- minted tokens are all sent to permissionedCandidatesValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ permissionedCandidatesOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [permissionedCandidateTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [permissionedCandidateTokenUtxo]
      )

permissionedCandidatesPolicyMintFailing01 :: TestTree
permissionedCandidatesPolicyMintFailing01 =
  expectFail "should fail if not signed by the governance authority" "ERROR-PERMISSIONED-CANDIDATES-POLICY-01" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesMint
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting permissionedCandidatesCurrSym
          -- not signed by governance (token missing):
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- minted tokens are all sent to permissionedCandidatesValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ permissionedCandidatesOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [permissionedCandidateTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [permissionedCandidateTokenUtxo]
      )

permissionedCandidatesPolicyMintFailing02 :: TestTree
permissionedCandidatesPolicyMintFailing02 =
  expectFail "should fail if some tokens are not sent to permissionedCandidatesValidatorAddress" "ERROR-PERMISSIONED-CANDIDATES-POLICY-02" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesMint
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting permissionedCandidatesCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- one minted token is sent to the wrong address:
          & _scriptContextTxInfo . _txInfoMint <>~ permissionedCandidatesOracleToken 2
          & _scriptContextTxInfo . _txInfoOutputs <>~ [permissionedCandidateTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [wrongAddressTokenUtxo]
      )
  where
    wrongCurrSym = V2.CurrencySymbol . V2.getScriptHash $ wrongValidatorScriptHash
    wrongValidatorScriptHash = V2.ScriptHash "wrongValidatorScriptHash"
    wrongOracleToken = V2.singleton wrongCurrSym (V2.TokenName "wrong token name") 1
    wrongValidatorAddress = V2.Address (V2.PubKeyCredential "01230123012301230123012301230123012301230123012301230123") Nothing

    wrongAddressTokenUtxo =
      mkTxOut
        wrongValidatorAddress
        wrongOracleToken
        Test.dummyBuiltinData
        wrongValidatorScriptHash

permissionedCandidatesPolicyBurnPassing :: TestTree
permissionedCandidatesPolicyBurnPassing =
  expectSuccess "should pass with burning a token" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesBurn
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting permissionedCandidatesCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- burns a permissioned candidate token
          & _scriptContextTxInfo . _txInfoMint <>~ permissionedCandidatesOracleToken (-1)
      )

permissionedCandidatesPolicyBurnFailing03 :: TestTree
permissionedCandidatesPolicyBurnFailing03 =
  expectFail "should fail if not signed by the governance authority" "ERROR-PERMISSIONED-CANDIDATES-POLICY-03" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesBurn
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting permissionedCandidatesCurrSym
          -- not signed by governance (token missing):
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
      )

permissionedCandidatesPolicyBurnFailing04 :: TestTree
permissionedCandidatesPolicyBurnFailing04 =
  expectFail "should fail if outputs PermissionedCandidatesTokens" "ERROR-PERMISSIONED-CANDIDATES-POLICY-04" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesBurn
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting permissionedCandidatesCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- mints a PermissionedCandidatesTokens:
          & _scriptContextTxInfo . _txInfoMint <>~ permissionedCandidatesOracleToken 1
          & _scriptContextTxInfo . _txInfoOutputs <>~ [permissionedCandidateTokenUtxo]
      )

permissionedCandidatesPolicyBurnFailing05 :: TestTree
permissionedCandidatesPolicyBurnFailing05 =
  expectFail "should fail if script purpose is not minting" "ERROR-PERMISSIONED-CANDIDATES-POLICY-05" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      permissionedCandidatesValidatorAddress
      Types.PermissionedCandidatesBurn
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

permissionedCandidateTokenUtxo :: V2.TxOut
permissionedCandidateTokenUtxo =
  mkTxOut
    permissionedCandidatesValidatorAddress
    (permissionedCandidatesOracleToken 1)
    Test.dummyBuiltinData
    permissionedCandidatesValidatorScriptHash

-- validator

validatorTests :: TestTree
validatorTests =
  testGroup
    "permissioned candidates validator"
    [ testGroup
        "update redeemer"
        [ permissionedCandidatesValidatorUpdatePassing
        , permissionedCandidatesValidatorUpdateFailing01
        ]
    , testGroup
        "remove redeemer"
        [ permissionedCandidatesValidatorRemovePassing
        , permissionedCandidatesValidatorRemoveFailing02
        ]
    ]

permissionedCandidatesValidatorUpdatePassing :: TestTree
permissionedCandidatesValidatorUpdatePassing =
  expectSuccess "should pass" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdatePermissionedCandidates
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

permissionedCandidatesValidatorUpdateFailing01 :: TestTree
permissionedCandidatesValidatorUpdateFailing01 =
  expectFail "should fail if not signed by the governance authority" "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-01" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdatePermissionedCandidates
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          -- not signed by governance (token missing):
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
      )

permissionedCandidatesValidatorRemovePassing :: TestTree
permissionedCandidatesValidatorRemovePassing =
  expectSuccess "should pass" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.RemovePermissionedCandidates
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

permissionedCandidatesValidatorRemoveFailing02 :: TestTree
permissionedCandidatesValidatorRemoveFailing02 =
  expectFail "should fail if not signed by the governance authority" "ERROR-PERMISSIONED-CANDIDATES-VALIDATOR-02" $
    runValidator
      Test.genesisUtxo
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.RemovePermissionedCandidates
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          -- not signed by governance (token missing):
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
      )

-- values

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

permissionedCandidatesValidatorAddress :: V2.Address
permissionedCandidatesValidatorAddress = V2.Address (V2.PubKeyCredential "890a890a890a890a890a890a890a890a890a890a890a890a890a890a") Nothing

permissionedCandidatesCurrSym :: V2.CurrencySymbol
permissionedCandidatesCurrSym = V2.CurrencySymbol . V2.getScriptHash $ permissionedCandidatesValidatorScriptHash

permissionedCandidatesValidatorScriptHash :: V2.ScriptHash
permissionedCandidatesValidatorScriptHash = V2.ScriptHash "permissionedCandidatesValidatorScriptHash"

permissionedCandidatesOracleTokenName :: V2.TokenName
permissionedCandidatesOracleTokenName = V2.TokenName "permissionedCandidates oracle"

permissionedCandidatesOracleToken :: Integer -> V2.Value
permissionedCandidatesOracleToken = V2.singleton permissionedCandidatesCurrSym permissionedCandidatesOracleTokenName

-- test runner

runMintingPolicy :: V2.TxOutRef -> Types.VersionOracleConfig -> V2.Address -> Types.PermissionedCandidatesPolicyRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy genesisUtxo vc permissionedCandidatesValidatorAddress' redeemer ctx =
  compiledMintingPolicy
    `appArg` genesisUtxo
    `appArg` vc
    `appArg` permissionedCandidatesValidatorAddress'
    `appArg` redeemer
    `appArg` ctx

runValidator :: V2.TxOutRef -> Types.VersionOracleConfig -> BuiltinData -> Types.PermissionedCandidatesValidatorRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator genesisUtxo vc datum redeemer ctx =
  compiledValidator
    `appArg` genesisUtxo
    `appArg` vc
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
