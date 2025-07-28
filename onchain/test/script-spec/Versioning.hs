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

tests :: TestTree
tests =
  testGroup
    "versioning policy"
    [ testGroup
        "initialize redeemer"
        [ versioningPolicyInitializePassing
        , versioningPolicyInitializeFailing01
        , versioningPolicyInitializeFailing02NoOutput
        , versioningPolicyInitializeFailing02NoDatum
        , versioningPolicyInitializeFailing02InvalidDatum
        , versioningPolicyInitializeFailing03
        ]
    , testGroup
        "mint redeemer"
        [ versioningPolicyMintPassing
        , versioningPolicyMintFailing04NoOutput
        , versioningPolicyMintFailing04NoDatum
        , versioningPolicyMintFailing04InvalidDatum
        , versioningPolicyMintFailing05
        , versioningPolicyMintFailing06
        ]
    , testGroup
        "burn redeemer"
        [ versioningPolicyBurnPassing
        , versioningPolicyBurnFailing07NoVersioningInput
        , versioningPolicyBurnFailing07VersioningInputHasNoDatum
        , versioningPolicyBurnFailing07VersioningInputHasInvalidDatum
        , versioningPolicyBurnFailing08
        , versioningPolicyBurnFailing09
        ]
    ]

-- init redeemer

versioningPolicyInitializePassing :: TestTree
versioningPolicyInitializePassing =
  expectSuccess "should pass" $
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
  expectFail "should fail on no genesis utxo (ERROR-VERSION-POLICY-01)" $
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
  expectFail "should fail on empty outputs (ERROR-VERSION-POLICY-02)" $
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
  expectFail "should fail on no datum in output (ERROR-VERSION-POLICY-02)" $
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
  expectFail "should fail on invalid datum in output (ERROR-VERSION-POLICY-02)" $
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
  expectFail "should fail on invalid token being minted (ERROR-VERSION-POLICY-03)" $
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
  expectSuccess "should pass" $
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

versioningPolicyMintFailing04NoOutput :: TestTree
versioningPolicyMintFailing04NoOutput =
  expectFail "should fail on empty outputs (ERROR-VERSION-POLICY-04)" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          -- no outputs provided
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyMintFailing04NoDatum :: TestTree
versioningPolicyMintFailing04NoDatum =
  expectFail "should fail on no datum in output (ERROR-VERSION-POLICY-04)" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          -- output has missing datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo & _txOutDatum .~ V2.NoOutputDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyMintFailing04InvalidDatum :: TestTree
versioningPolicyMintFailing04InvalidDatum =
  expectFail "should fail on invalid datum in output (ERROR-VERSION-POLICY-04)" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          -- output has invalid datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo & _txOutDatum .~ V2.OutputDatum invalidDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )
  where
    invalidDatum = V2.Datum $ toBuiltinData (0 :: Integer)

versioningPolicyMintFailing05 :: TestTree
versioningPolicyMintFailing05 =
  expectFail "should fail if not signed by gov auth (ERROR-VERSION-POLICY-05)" $
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
  expectFail "should fail on invalid token being minted (ERROR-VERSION-POLICY-06)" $
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
  expectSuccess "should pass" $
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

versioningPolicyBurnFailing07NoVersioningInput :: TestTree
versioningPolicyBurnFailing07NoVersioningInput =
  expectFail "should fail on missing versioning utxo in inputs (ERROR-VERSION-POLICY-07)" $
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

versioningPolicyBurnFailing07VersioningInputHasNoDatum :: TestTree
versioningPolicyBurnFailing07VersioningInputHasNoDatum =
  expectFail "should fail on versioning utxo with no datum in inputs (ERROR-VERSION-POLICY-07)" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          -- versioning utxo has missing datum:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ (versioningTokenUtxo & _txOutDatum .~ V2.NoOutputDatum)]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing07VersioningInputHasInvalidDatum :: TestTree
versioningPolicyBurnFailing07VersioningInputHasInvalidDatum =
  expectFail "should fail on versioning utxo with invalid datum in inputs (ERROR-VERSION-POLICY-07)" $
    runVersioningPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          -- versioning utxo has invalid datum:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ (versioningTokenUtxo & _txOutDatum .~ V2.OutputDatum invalidDatum)]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )
  where
    invalidDatum = V2.Datum $ toBuiltinData (0 :: Integer)

versioningPolicyBurnFailing08 :: TestTree
versioningPolicyBurnFailing08 =
  expectFail "should fail if versioning utxo is in output (ERROR-VERSION-POLICY-08)" $
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
  expectFail "should fail if not signed by gov authority (ERROR-VERSION-POLICY-09)" $
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
