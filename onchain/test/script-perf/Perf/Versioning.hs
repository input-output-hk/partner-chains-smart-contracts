module Perf.Versioning (execCosts) where

import Prelude

import ApiBuilder
import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import Test.Tasty
import TestValues qualified as Test
import Testing
import TrustlessSidechain.Scripts.Versioning
import TrustlessSidechain.Types qualified as Types

execCosts :: TestTree
execCosts =
  testGroup
    "Versioning scripts ExBudget"
    [ goldenPerf "versioningPolicyInitialize" versioningPolicyInitialize
    , goldenPerf "versioningPolicyMint" versioningPolicyMint
    , goldenPerf "versioningPolicyBurn" versioningPolicyBurn
    , goldenPerf "versioningValidator" versioningValidator
    ]

versioningPolicyInitialize :: CompiledCode BuiltinUnit
versioningPolicyInitialize =
  runMintingPolicy
    Test.genesisUtxo
    Test.versionValidatorAddress
    (Types.InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
        & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
    )

versioningPolicyMint :: CompiledCode BuiltinUnit
versioningPolicyMint =
  runMintingPolicy
    Test.genesisUtxo
    Test.versionValidatorAddress
    (Types.MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
        -- governance version oracle:
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
        & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
    )

versioningPolicyBurn :: CompiledCode BuiltinUnit
versioningPolicyBurn =
  runMintingPolicy
    Test.genesisUtxo
    Test.versionValidatorAddress
    (Types.BurnVersionOracle Test.versionOracle)
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.versioningTokenUtxo]
        & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
    )

versioningValidator :: CompiledCode BuiltinUnit
versioningValidator =
  runValidator
    Test.genesisUtxo
    versionOracleDatum
    Test.versionOracle
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Spending versioningUtxo
        -- signed by governance:
        & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
        & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
        -- [INPUT] Versioning UTXO:
        & _scriptContextTxInfo . _txInfoInputs
          <>~ [ emptyTxInInfo
                  & _txInInfoOutRef .~ versioningUtxo
                  & _txInInfoResolved
                    .~ ( emptyTxOut
                          & _txOutAddress .~ versionOracleAddress
                       )
              ]
        -- [OUTPUT] Versioning UTXO:
        & _scriptContextTxInfo . _txInfoOutputs
          <>~ [ emptyTxOut
                  & _txOutAddress .~ versionOracleAddress
                  & _txOutValue <>~ Test.versionOracleToken 1
              ]
        -- [OUTPUT] Some other UTXO:
        & _scriptContextTxInfo . _txInfoOutputs
          <>~ [ emptyTxOut
                  & _txOutAddress .~ someAddress
                  -- non versioning output cannot have versioning token
              ]
    )

-- test values

versionOracleDatum :: Types.VersionOracleDatum
versionOracleDatum =
  Types.VersionOracleDatum
    (Test.versionOracle)
    (Test.toAsData Test.versioningCurrSym)

versioningUtxo :: V2.TxOutRef
versioningUtxo = V2.TxOutRef "98769876" 99

versionOracleAddress :: V2.Address
versionOracleAddress = V2.Address (V2.PubKeyCredential "98769876987698769876987698769876987698769876987698769876") Nothing

someAddress :: V2.Address
someAddress = V2.Address (V2.PubKeyCredential "09870987098709870987098709870987098709870987098709870987") Nothing

-- test runner

runMintingPolicy :: V2.TxOutRef -> V2.Address -> Types.VersionOraclePolicyRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy genesisUtxo validatorAddress redeemer ctx =
  compiledVersionOraclePolicy
    `appArg` genesisUtxo
    `appArg` validatorAddress
    `appArg` redeemer
    `appArg` ctx

runValidator :: V2.TxOutRef -> Types.VersionOracleDatum -> Types.VersionOracle -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator genesisUtxo datum redeemer ctx =
  compiledVersionOracleValidator
    `appArg` genesisUtxo
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
