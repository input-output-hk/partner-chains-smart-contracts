module Specs.Versioning where

import ApiBuilder
import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import Test.Tasty
import TestValues qualified as Test
import Testing
import TrustlessSidechain.Types
import TrustlessSidechain.Types qualified as Types
import TrustlessSidechain.Versioning

-- minting policy

policyTests :: TestTree
policyTests =
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
    , versioningPolicyNotMintFailing
    ]

-- init redeemer

versioningPolicyInitializePassing :: TestTree
versioningPolicyInitializePassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyInitializeFailing01 :: TestTree
versioningPolicyInitializeFailing01 =
  expectFail "should fail on no genesis utxo" "ERROR-VERSION-POLICY-01" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- no genesis utxo in inputs
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyInitializeFailing02NoOutput :: TestTree
versioningPolicyInitializeFailing02NoOutput =
  expectFail "should fail on empty outputs" "ERROR-VERSION-POLICY-02" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          -- no output
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyInitializeFailing02NoDatum :: TestTree
versioningPolicyInitializeFailing02NoDatum =
  expectFail "should fail on no datum in output" "ERROR-VERSION-POLICY-02" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          -- output has missing datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo & _txOutDatum .~ V2.NoOutputDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyInitializeFailing02InvalidDatum :: TestTree
versioningPolicyInitializeFailing02InvalidDatum =
  expectFail "should fail on invalid datum in output" "ERROR-VERSION-POLICY-02" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          -- output has invalid datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo & _txOutDatum .~ V2.OutputDatum invalidDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )
  where
    invalidDatum = V2.Datum $ toBuiltinData (0 :: Integer)

versioningPolicyInitializeFailing03 :: TestTree
versioningPolicyInitializeFailing03 =
  expectFail "should fail on invalid token being minted" "ERROR-VERSION-POLICY-03" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint .~ Test.wrongToken
      )

-- mint redeemer

versioningPolicyMintPassing :: TestTree
versioningPolicyMintPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyMintFailing04NoOutput :: TestTree
versioningPolicyMintFailing04NoOutput =
  expectFail "should fail on empty outputs" "ERROR-VERSION-POLICY-04" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- no outputs provided
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyMintFailing04NoDatum :: TestTree
versioningPolicyMintFailing04NoDatum =
  expectFail "should fail on no datum in output" "ERROR-VERSION-POLICY-04" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- output has missing datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo & _txOutDatum .~ V2.NoOutputDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyMintFailing04InvalidDatum :: TestTree
versioningPolicyMintFailing04InvalidDatum =
  expectFail "should fail on invalid datum in output" "ERROR-VERSION-POLICY-04" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- output has invalid datum:
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo & _txOutDatum .~ V2.OutputDatum invalidDatum]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )
  where
    invalidDatum = V2.Datum $ toBuiltinData (0 :: Integer)

versioningPolicyMintFailing05 :: TestTree
versioningPolicyMintFailing05 =
  expectFail "should fail if governance approval is not present" "ERROR-VERSION-POLICY-05" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] not signed by governance
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

versioningPolicyMintFailing06 :: TestTree
versioningPolicyMintFailing06 =
  expectFail "should fail on invalid token being minted" "ERROR-VERSION-POLICY-06" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.wrongToken
      )

-- burn redeemer

versioningPolicyBurnPassing :: TestTree
versioningPolicyBurnPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing07NoVersioningInput :: TestTree
versioningPolicyBurnFailing07NoVersioningInput =
  expectFail "should fail on missing versioning utxo in inputs" "ERROR-VERSION-POLICY-07" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- no versioning utxo in inputs
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing07VersioningInputHasNoDatum :: TestTree
versioningPolicyBurnFailing07VersioningInputHasNoDatum =
  expectFail "should fail on versioning utxo with no datum in inputs" "ERROR-VERSION-POLICY-07" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- versioning utxo has missing datum:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ (Test.versioningTokenUtxo & _txOutDatum .~ V2.NoOutputDatum)]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing07VersioningInputHasInvalidDatum :: TestTree
versioningPolicyBurnFailing07VersioningInputHasInvalidDatum =
  expectFail "should fail on versioning utxo with invalid datum in inputs" "ERROR-VERSION-POLICY-07" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- versioning utxo has invalid datum:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ (Test.versioningTokenUtxo & _txOutDatum .~ V2.OutputDatum invalidDatum)]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )
  where
    invalidDatum = V2.Datum $ toBuiltinData (0 :: Integer)

versioningPolicyBurnFailing08 :: TestTree
versioningPolicyBurnFailing08 =
  expectFail "should fail if versioning utxo is in output" "ERROR-VERSION-POLICY-08" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo] -- shouldn't be present
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

versioningPolicyBurnFailing09 :: TestTree
versioningPolicyBurnFailing09 =
  expectFail "should fail if governance approval is not present" "ERROR-VERSION-POLICY-09" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (BurnVersionOracle Test.versionOracle)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] not signed by governance
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.versioningTokenUtxo]
      )

versioningPolicyNotMintFailing :: TestTree
versioningPolicyNotMintFailing =
  expectFail "should fail if script purpose is not minting" "ERROR-VERSION-POLICY-10" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionValidatorAddress
      (InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash)
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending (V2.TxOutRef "abcd0123" 0)
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [Test.versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken 1
      )

-- validator

validatorTests :: TestTree
validatorTests =
  testGroup
    "versioning oracle validator"
    [ versioningValidatorPassing
    , versioningValidatorFailing01
    , versioningValidatorFailing02
    , versioningValidatorFailing03
    , versioningValidatorFailing04
    , versioningValidatorFailing05
    ]

versioningValidatorPassing :: TestTree
versioningValidatorPassing =
  expectSuccess "should pass" $
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

versioningValidatorFailing01 :: TestTree
versioningValidatorFailing01 =
  expectFail "should fail if governance approval is not present" "ERROR-VERSION-VALIDATOR-01" $
    runValidator
      Test.genesisUtxo
      versionOracleDatum
      Test.versionOracle
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending versioningUtxo
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] not signed by governance
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

versioningValidatorFailing02 :: TestTree
versioningValidatorFailing02 =
  expectFail "should fail if version oracle in the datum does not match the redeemer" "ERROR-VERSION-VALIDATOR-02" $
    runValidator
      Test.genesisUtxo
      -- [ERROR] version oracle does not match one in datum:
      wrongVersionOracleDatum
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
  where
    wrongVersionOracleDatum :: VersionOracleDatum
    wrongVersionOracleDatum =
      VersionOracleDatum
        wrongVersionOracle
        (Test.toAsData Test.versioningCurrSym)

    wrongVersionOracle :: Types.VersionOracle
    wrongVersionOracle = Types.VersionOracle 77

versioningValidatorFailing03 :: TestTree
versioningValidatorFailing03 =
  expectFail "should fail if transaction outputs version tokens to non-versioning address" "ERROR-VERSION-VALIDATOR-03" $
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
                    -- [ERROR] versioning token is leaked
                    & _txOutValue <>~ Test.versionOracleToken 1
                ]
      )

versioningValidatorFailing04 :: TestTree
versioningValidatorFailing04 =
  expectFail "should fail if invalid script purpose is provided" "ERROR-VERSION-VALIDATOR-04" $
    runValidator
      Test.genesisUtxo
      versionOracleDatum
      Test.versionOracle
      ( emptyScriptContext
          -- [ERROR] minting script purpose instead of spending
          & _scriptContextPurpose .~ V2.Minting (V2.CurrencySymbol "someCurrSym")
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

versioningValidatorFailing05 :: TestTree
versioningValidatorFailing05 =
  expectFail "should fail if transaction does not have own input" "ERROR-VERSION-VALIDATOR-05" $
    runValidator
      Test.genesisUtxo
      versionOracleDatum
      Test.versionOracle
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending versioningUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- [ERROR] no input
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

versionOracleDatum :: VersionOracleDatum
versionOracleDatum =
  VersionOracleDatum
    (Test.versionOracle)
    (Test.toAsData Test.versioningCurrSym)

versioningUtxo :: V2.TxOutRef
versioningUtxo = V2.TxOutRef "98769876" 99

versionOracleAddress :: V2.Address
versionOracleAddress = V2.Address (V2.PubKeyCredential "98769876987698769876987698769876987698769876987698769876") Nothing

someAddress :: V2.Address
someAddress = V2.Address (V2.PubKeyCredential "09870987098709870987098709870987098709870987098709870987") Nothing

-- test runner

runMintingPolicy :: V2.TxOutRef -> V2.Address -> VersionOraclePolicyRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy genesisUtxo validatorAddress redeemer ctx =
  compiledVersionOraclePolicy
    `appArg` genesisUtxo
    `appArg` validatorAddress
    `appArg` redeemer
    `appArg` ctx

runValidator :: V2.TxOutRef -> VersionOracleDatum -> VersionOracle -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator genesisUtxo datum redeemer ctx =
  compiledVersionOracleValidator
    `appArg` genesisUtxo
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
