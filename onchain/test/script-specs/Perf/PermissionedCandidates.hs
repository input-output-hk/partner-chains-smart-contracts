module Perf.PermissionedCandidates (execCosts) where

import ApiBuilder
import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import Test.Tasty
import TestValues qualified as Test
import Testing
import TrustlessSidechain.PermissionedCandidates
import TrustlessSidechain.Types qualified as Types

execCosts :: TestTree
execCosts =
  testGroup
    "PermissionedCandidates scripts ExBudget"
    [ goldenPerf "permissionedCandidatesPolicyBurn" permissionedCandidatesPolicyBurn
    , goldenPerf "permissionedCandidatesPolicyMint" permissionedCandidatesPolicyMint
    , goldenPerf "permissionedCandidatesValidatorRemove" permissionedCandidatesValidatorRemove
    , goldenPerf "permissionedCandidatesValidatorUpdate" permissionedCandidatesValidatorUpdate
    ]

permissionedCandidatesPolicyBurn :: CompiledCode BuiltinUnit
permissionedCandidatesPolicyBurn =
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

permissionedCandidatesPolicyMint :: CompiledCode BuiltinUnit
permissionedCandidatesPolicyMint =
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

permissionedCandidateTokenUtxo :: V2.TxOut
permissionedCandidateTokenUtxo =
  mkTxOut
    permissionedCandidatesValidatorAddress
    (permissionedCandidatesOracleToken 1)
    Test.dummyBuiltinData
    permissionedCandidatesValidatorScriptHash

permissionedCandidatesValidatorUpdate :: CompiledCode BuiltinUnit
permissionedCandidatesValidatorUpdate =
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

permissionedCandidatesValidatorRemove :: CompiledCode BuiltinUnit
permissionedCandidatesValidatorRemove =
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
