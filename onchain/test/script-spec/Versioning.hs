module Versioning where

import Control.Lens
import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.Types
import TrustlessSidechain.Versioning
import Prelude

-- init redeemer

versioningPolicyInitializePassing :: TestTree
versioningPolicyInitializePassing =
  expectSuccess "init versioning policy should pass" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
      )

versioningPolicyInitializeFailing01 :: TestTree
versioningPolicyInitializeFailing01 =
  expectFail "init versioning policy should fail on no genesis utxo" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- no genesis utxo in inputs
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
      )

versioningPolicyInitializeFailing02NoOutput :: TestTree
versioningPolicyInitializeFailing02NoOutput =
  expectFail "init versioning policy should fail on empty outputs" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          -- no output
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
      )

versioningPolicyInitializeFailing02NoDatum :: TestTree
versioningPolicyInitializeFailing02NoDatum =
  expectFail "init versioning policy should fail on no datum in output" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          -- output has missing datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo & _txOutDatum .~ V2.NoOutputDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
      )

versioningPolicyInitializeFailing02InvalidDatum :: TestTree
versioningPolicyInitializeFailing02InvalidDatum =
  expectFail "init versioning policy should fail on invalid datum in output" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          -- output has invalid datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo & _txOutDatum .~ V2.OutputDatum invalidDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
      )
  where
    invalidDatum = V2.Datum $ toBuiltinData (0 :: Integer)

versioningPolicyInitializeFailing03 :: TestTree
versioningPolicyInitializeFailing03 =
  expectFail "init versioning policy should fail on invalid token being minted" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoMint .~ Test.wrongToken
      )

-- mint redeemer

versioningPolicyMintPassing :: TestTree
versioningPolicyMintPassing =
  expectSuccess "mint versioning policy should pass" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyMintFailing04 :: TestTree
versioningPolicyMintFailing04 =
  expectFail "mint versioning policy should fail on empty inputs" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- no inputs provided
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyMintFailing05 :: TestTree
versioningPolicyMintFailing05 =
  expectFail "mint versioning policy should fail if not signed by gov auth" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- gov token not in inputs
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyMintFailing06 :: TestTree
versioningPolicyMintFailing06 =
  expectFail "mint versioning policy should fail on invalid token being minted" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.wrongToken
      )

-- burn redeemer

versioningPolicyBurnPassing :: TestTree
versioningPolicyBurnPassing =
  expectSuccess "burn versioning policy should pass" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing07 :: TestTree
versioningPolicyBurnFailing07 =
  expectFail "burn versioning policy should fail on missing versioning utxo in inputs" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          -- no versioning utxo in inputs
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing08 :: TestTree
versioningPolicyBurnFailing08 =
  expectFail "burn versioning policy should fail if versioning utxo is in output" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo] -- shouldn't be present
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing09 :: TestTree
versioningPolicyBurnFailing09 =
  expectFail "burn versioning policy should fail if not signed by gov authority" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ versioningTokenUtxo]
      )

-- governance utxo input and tokens are missing

versioningTokenUtxo :: V2.TxOut
versioningTokenUtxo =
  mkTxOut
    Test.versionValidatorAddress
    Test.versionOracleToken
    Test.versionOracleDatum
    Test.versioningValidatorScriptHash

governanceTokenUtxo :: V2.TxOut
governanceTokenUtxo =
  mkTxOut
    Test.versionValidatorAddress
    Test.versionOracleToken
    Test.governanceVersionOracleDatum
    Test.governanceValidatorScriptHash

-- test runner

runVersioningPolicy :: V2.TxOutRef -> V2.Address -> VersionOraclePolicyRedeemer -> V2.ScriptContext -> BuiltinUnit
runVersioningPolicy genesisUtxo validatorAddress redeemer ctx =
  mkVersionOraclePolicyUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData validatorAddress)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)
