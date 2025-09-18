module Specs.Reserve (
  policyTests,
  validatorTests,
) where

import ApiBuilder
import Control.Lens
import PlutusLedgerApi.V1.Address qualified as Address
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import Test.Tasty
import TestValues qualified as Test
import Testing
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Scripts.Reserve
import TrustlessSidechain.Types qualified as Types

-- minting policy

policyTests :: TestTree
policyTests =
  testGroup
    "reserve auth policy"
    [ reserveAuthPolicyBurnPassing
    , reserveAuthPolicyPassing
    , reserveAuthPolicyFailing01
    , reserveAuthPolicyFailing02
    , reserveAuthPolicyFailing03
    , reserveAuthPolicyFailing04
    , reserveAuthPolicyFailing05
    , reserveAuthPolicyFailing06
    ]

reserveAuthPolicyBurnPassing :: TestTree
reserveAuthPolicyBurnPassing =
  expectSuccess "burn should pass" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- reserve auth token burned
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken (-1)
      )

reserveAuthPolicyPassing :: TestTree
reserveAuthPolicyPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- reserve auth token minted
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken 1
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned initialReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )

reserveAuthPolicyFailing01 :: TestTree
reserveAuthPolicyFailing01 =
  expectFail "should fail if not approved by governance" "ERROR-RESERVE-AUTH-01" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] Governance token missing
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- reserve auth token minted
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken 1
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned initialReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )

reserveAuthPolicyFailing02 :: TestTree
reserveAuthPolicyFailing02 =
  expectFail "should fail if single reserve authentication token is not minted" "ERROR-RESERVE-AUTH-02" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- [ERROR] reserve auth token not minted
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned initialReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )

reserveAuthPolicyFailing03 :: TestTree
reserveAuthPolicyFailing03 =
  expectFail "should fail if output reserve UTxO doesn't carry auth token" "ERROR-RESERVE-AUTH-03" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- reserve auth token minted
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken 1
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned initialReserveDatum)
                    -- [ERROR] does not carry reserve auth token
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )

reserveAuthPolicyFailing04 :: TestTree
reserveAuthPolicyFailing04 =
  expectFail "should fail if output reserve UTxO doesn't carry correct initial datum" "ERROR-RESERVE-AUTH-04" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- reserve auth token minted
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken 1
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    -- [ERROR] carries incorrect initial datum
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned incorrectInitialReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )
  where
    -- incorrect because total amount transferred is not 0
    incorrectInitialReserveDatum :: Types.ReserveDatum
    incorrectInitialReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 1}
        }

reserveAuthPolicyFailing05 :: TestTree
reserveAuthPolicyFailing05 =
  expectFail "should fail if no unique output UTxO at the reserve address" "ERROR-RESERVE-AUTH-05" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- reserve auth token minted
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken 1
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned initialReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
          -- [ERROR] Another Reserve UTXO at reserve address:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned initialReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )

reserveAuthPolicyFailing06 :: TestTree
reserveAuthPolicyFailing06 =
  expectFail "should fail if output reserve UTxO carries no inline datum or malformed datum" "ERROR-RESERVE-AUTH-06" $
    runMintingPolicy
      Test.versionOracleConfig
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting reserveAuthPolicyCurrencySymbol
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveValidatorVersionOracleUtxo]
          -- reserve auth token minted
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken 1
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    -- [ERROR] output Reserve UTXO carries no datum
                    & _txOutDatum .~ V2.NoOutputDatum
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- PC tokens:
                    & _txOutValue <>~ partnerToken 5
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 5
                ]
      )

-- validator

validatorTests :: TestTree
validatorTests =
  testGroup
    "reserve validator"
    [ testGroup
        "deposit redeemer"
        [ reserveValidatorDepositPassing
        , reserveValidatorDepositFailing01
        , reserveValidatorDepositFailing02
        , reserveValidatorDepositFailing03
        ]
    , testGroup
        "update redeemer"
        [ reserveValidatorUpdatePassing
        , reserveValidatorUpdateFailing04
        , reserveValidatorUpdateFailing05
        , reserveValidatorUpdateFailing06
        , reserveValidatorUpdateFailing07
        , reserveValidatorUpdateFailing08
        , reserveValidatorUpdateFailing09
        , reserveValidatorUpdateFailing10
        , reserveValidatorUpdateFailing18
        ]
    , testGroup
        "transfer to ics redeemer"
        [ reserveValidatorTransferToICSPassing
        , reserveValidatorTransferToICSFailing11
        , reserveValidatorTransferToICSFailing12
        , reserveValidatorTransferToICSFailing13
        , reserveValidatorTransferToICSFailing14
        ]
    , testGroup
        "handover redeemer"
        [ reserveValidatorHandoverPassing
        , reserveValidatorHandoverFailing15
        , reserveValidatorHandoverFailing16
        , reserveValidatorHandoverFailing17
        ]
    ]

-- deposit redeemer

reserveValidatorDepositPassing :: TestTree
reserveValidatorDepositPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.DepositToReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- input utxo for depositing into reserve: (needed for balancing, not for validator to pass)
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ someUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ someAddress
                            -- tokens to be deposited into reserve:
                            & _txOutValue <>~ partnerToken 50
                         )
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                            -- spare ADA:
                            & _txOutValue <>~ Test.mkAdaToken 5
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- carries more partner tokens than the input:
                    & _txOutValue <>~ partnerToken 60
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 6
                ]
      )

reserveValidatorDepositFailing01 :: TestTree
reserveValidatorDepositFailing01 =
  expectFail "should fail if governance approval is not present" "ERROR-RESERVE-01" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.DepositToReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] NOT signed by governance
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- input utxo for depositing into reserve: (needed for balancing, not for validator to pass)
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ someUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ someAddress
                            -- tokens to be deposited into reserve:
                            & _txOutValue <>~ partnerToken 50
                         )
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- carries more partner tokens than the input:
                    & _txOutValue <>~ partnerToken 60
                ]
      )

reserveValidatorDepositFailing02 :: TestTree
reserveValidatorDepositFailing02 =
  expectFail "should fail if datum of the propagated reserve utxo changes" "ERROR-RESERVE-02" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.DepositToReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- input utxo for depositing into reserve: (needed for balancing, not for validator to pass)
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ someUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ someAddress
                            -- tokens to be deposited into reserve:
                            & _txOutValue <>~ partnerToken 50
                         )
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    -- [ERROR] reserve datum changes:
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned changedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- carries more partner tokens than the input:
                    & _txOutValue <>~ partnerToken 60
                ]
      )
  where
    changedReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 15}
        }

reserveValidatorDepositFailing03 :: TestTree
reserveValidatorDepositFailing03 =
  expectFail "should fail if assets of the propagated reserve utxo don't increase by reserve tokens" "ERROR-RESERVE-03" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.DepositToReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- input utxo for depositing into reserve: (needed for balancing, not for validator to pass)
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ someUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ someAddress
                            -- tokens to be deposited into reserve:
                            & _txOutValue <>~ partnerToken 50
                         )
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                            -- spare ADA:
                            & _txOutValue <>~ Test.mkAdaToken 5
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- [ERROR] tokens in reserve don't increase:
                    & _txOutValue <>~ partnerToken 10
                    -- spare ADA:
                    & _txOutValue <>~ Test.mkAdaToken 6
                ]
      )

-- update redeemer

reserveValidatorUpdatePassing :: TestTree
reserveValidatorUpdatePassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorUpdateFailing04 :: TestTree
reserveValidatorUpdateFailing04 =
  expectFail "should fail if no unique input utxo carrying authentication token" "ERROR-RESERVE-04" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [ERROR] No unique input reserve UTXO:
          -- [INPUT] Another Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorUpdateFailing05 :: TestTree
reserveValidatorUpdateFailing05 =
  expectFail "should fail if no unique output utxo at the reserve address and carrying authentication token" "ERROR-RESERVE-05" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
          -- [ERROR] No unique output reserve UTXO:
          -- [OUTPUT] Another Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorUpdateFailing06 :: TestTree
reserveValidatorUpdateFailing06 =
  expectFail "should fail if datum of input reserve utxo malformed" "ERROR-RESERVE-06" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            -- [ERROR] malformed reserve datum on input:
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned ())
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorUpdateFailing07 :: TestTree
reserveValidatorUpdateFailing07 =
  expectFail "should fail if datum of output reserve utxo malformed" "ERROR-RESERVE-07" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    -- [ERROR] malformed reserve datum on output:
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned ())
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )

reserveValidatorUpdateFailing08 :: TestTree
reserveValidatorUpdateFailing08 =
  expectFail "should fail if governance approval is not present" "ERROR-RESERVE-08" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] not signed by governance
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorUpdateFailing09 :: TestTree
reserveValidatorUpdateFailing09 =
  expectFail "should fail if datum of the propagated reserve utxo changes not only by immutable settings" "ERROR-RESERVE-09" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    -- [ERROR] immutable settings change:
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned invalidUpdatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    invalidUpdatedReserveDatum =
      reserveDatum
        { Types.immutableSettings =
            Types.ImmutableReserveSettings
              { tokenKind = Test.toAsData someAssetClass
              }
        }

reserveValidatorUpdateFailing10 :: TestTree
reserveValidatorUpdateFailing10 =
  expectFail "should fail if assets of the propagated reserve utxo change" "ERROR-RESERVE-10" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- [ERROR] token amount changes:
                    & _txOutValue <>~ partnerToken 11
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorUpdateFailing18 :: TestTree
reserveValidatorUpdateFailing18 =
  expectFail "should fail if continuing output exists without an authentication token" "ERROR-RESERVE-18" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.UpdateReserve
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned updatedReserveDatum)
                    -- [ERROR] missing reserve auth token
                    -- token amount doesn't change:
                    & _txOutValue <>~ partnerToken 10
                ]
      )
  where
    updatedReserveDatum =
      reserveDatum
        { Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

-- transfer to ICS redeemer

reserveValidatorTransferToICSPassing :: TestTree
reserveValidatorTransferToICSPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.TransferToIlliquidCirculationSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- v-function tokens minted:
          & _scriptContextTxInfo . _txInfoMint <>~ vFunctionToken 15
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            -- the ICS has 5 PC tokens
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    -- the ICS increases by 4 tokens (5 is released, fee is 1)
                    & _txOutValue <>~ partnerToken 9
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned inputReserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- reserve has 10 PC tokens
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned outputReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- the PC tokens in the reserve decrease to 5 as 5 is released
                    & _txOutValue <>~ partnerToken 5
                ]
          -- [OUTPUT] fee UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ someAddress
                    -- receives 1 PC token as fee
                    & _txOutValue <>~ partnerToken 1
                ]
      )
  where
    inputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 10}
        }
    outputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 15}
        }

reserveValidatorTransferToICSFailing11 :: TestTree
reserveValidatorTransferToICSFailing11 =
  expectFail "should fail if assets of the propagated reserve utxo don't decrease by reserve tokens in desired way" "ERROR-RESERVE-11" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.TransferToIlliquidCirculationSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- v-function tokens minted:
          & _scriptContextTxInfo . _txInfoMint <>~ vFunctionToken 15
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            -- the ICS has 5 PC tokens
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    -- the ICS increases by 4 tokens (5 is released, fee is 1)
                    & _txOutValue <>~ partnerToken 9
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned inputReserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- reserve has 10 PC tokens
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned outputReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- [ERROR] the PC tokens in the reserve decrease to 6 instead of the expected 5:
                    & _txOutValue <>~ partnerToken 6
                ]
          -- [OUTPUT] fee UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ someAddress
                    -- receives 1 PC token as fee
                    & _txOutValue <>~ partnerToken 1
                ]
      )
  where
    inputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 10}
        }
    outputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 15}
        }

reserveValidatorTransferToICSFailing12 :: TestTree
reserveValidatorTransferToICSFailing12 =
  expectFail "should fail if datum of the propagated reserve utxo changes not only by stats in desired way" "ERROR-RESERVE-12" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.TransferToIlliquidCirculationSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- v-function tokens minted:
          & _scriptContextTxInfo . _txInfoMint <>~ vFunctionToken 15
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            -- the ICS has 5 PC tokens
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    -- the ICS increases by 4 tokens (5 is released, fee is 1)
                    & _txOutValue <>~ partnerToken 9
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned inputReserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- reserve has 10 PC tokens
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    -- [ERROR] mutable settings change too in reserve output datum, when only stats are expected
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned invalidOutputReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- the PC tokens in the reserve decrease to 5 as 5 is released
                    & _txOutValue <>~ partnerToken 5
                ]
          -- [OUTPUT] fee UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ someAddress
                    -- receives 1 PC token as fee
                    & _txOutValue <>~ partnerToken 1
                ]
      )
  where
    inputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 10}
        }
    invalidOutputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 15}
        , Types.mutableSettings =
            Types.MutableReserveSettings
              { vFunctionTotalAccrued = Test.toAsData $ V2.CurrencySymbol "newVFunctionScriptHash"
              , incentiveAmount = 2
              }
        }

reserveValidatorTransferToICSFailing13 :: TestTree
reserveValidatorTransferToICSFailing13 =
  expectFail "should fail if incorrect amount of reserve tokens goes into an illiquid circulation supply" "ERROR-RESERVE-13" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.TransferToIlliquidCirculationSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- v-function tokens minted:
          & _scriptContextTxInfo . _txInfoMint <>~ vFunctionToken 15
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            -- the ICS has 5 PC tokens
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    -- [ERROR] the ICS increases by 5 tokens (expected is 4 as 5 is released and fee is 1)
                    & _txOutValue <>~ partnerToken 10
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned inputReserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- reserve has 10 PC tokens
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned outputReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- the PC tokens in the reserve decrease to 5 as 5 is released
                    & _txOutValue <>~ partnerToken 5
                ]
          -- [OUTPUT] fee UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ someAddress
                    -- receives 1 PC token as fee
                    & _txOutValue <>~ partnerToken 1
                ]
      )
  where
    inputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 10}
        }
    outputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 15}
        }

reserveValidatorTransferToICSFailing14 :: TestTree
reserveValidatorTransferToICSFailing14 =
  expectFail "should fail if no unique output utxo at the illiquid circulation supply address" "ERROR-RESERVE-14" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.TransferToIlliquidCirculationSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- v-function tokens minted:
          & _scriptContextTxInfo . _txInfoMint <>~ vFunctionToken 15
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            -- the ICS has 5 PC tokens
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    -- the ICS increases by 4 tokens (5 is released, fee is 1)
                    & _txOutValue <>~ partnerToken 9
                ]
          -- [ERROR] no unique ICS UTXO output:
          -- [OUTPUT] Another ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    -- the ICS increases by 4 tokens (5 is released, fee is 1)
                    & _txOutValue <>~ partnerToken 9
                ]
          -- [INPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned inputReserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- reserve has 10 PC tokens
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
          -- [OUTPUT] Reserve UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ reserveAddress
                    & _txOutDatum .~ V2.OutputDatum (wrapToVersioned outputReserveDatum)
                    -- carries reserve auth token:
                    & _txOutValue <>~ reserveAuthToken 1
                    -- the PC tokens in the reserve decrease to 5 as 5 is released
                    & _txOutValue <>~ partnerToken 5
                ]
          -- [OUTPUT] fee UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ someAddress
                    -- receives 1 PC token as fee
                    & _txOutValue <>~ partnerToken 1
                ]
      )
  where
    inputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 10}
        }
    outputReserveDatum =
      reserveDatum
        { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 15}
        }

-- handover redeemer

reserveValidatorHandoverPassing :: TestTree
reserveValidatorHandoverPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.Handover
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- reserve auth token burned
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken (-1)
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    & _txOutValue <>~ partnerToken 15
                ]
          -- [INPUT] Reserve UTXO:
          -- Note: There is no Reserve UTXO output
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
      )

reserveValidatorHandoverFailing15 :: TestTree
reserveValidatorHandoverFailing15 =
  expectFail "should fail if governance approval is not present" "ERROR-RESERVE-15" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.Handover
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- governance version oracle:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- [ERROR] not signed by governance
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- reserve auth token burned
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken (-1)
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    & _txOutValue <>~ partnerToken 15
                ]
          -- [INPUT] Reserve UTXO:
          -- Note: There is no Reserve UTXO output
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
      )

reserveValidatorHandoverFailing16 :: TestTree
reserveValidatorHandoverFailing16 =
  expectFail "should fail if an authentication token is not burnt" "ERROR-RESERVE-16" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.Handover
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- [ERROR] reserve auth token is NOT burned
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    & _txOutValue <>~ partnerToken 15
                ]
          -- [INPUT] Reserve UTXO:
          -- Note: There is no Reserve UTXO output
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
      )

reserveValidatorHandoverFailing17 :: TestTree
reserveValidatorHandoverFailing17 =
  expectFail "should fail if not all reserve tokens are transferred to illiquid circulation supply" "ERROR-RESERVE-17" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      Types.Handover
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending reserveUtxo
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- ReserveAuthPolicy VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ reserveAuthPolicyVersionOracleUtxo]
          -- IlliquidCirculationSupplyValidator VersionOracle
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsSupplyValidatorVersionOracleUtxo]
          -- reserve auth token burned
          & _scriptContextTxInfo . _txInfoMint <>~ reserveAuthToken (-1)
          -- [INPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ icsUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ icsAddress
                            & _txOutValue <>~ partnerToken 5
                         )
                ]
          -- [OUTPUT] ICS UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ icsAddress
                    & _txOutValue <>~ partnerToken 14
                ]
          -- [ERROR] not all tokens are transferred to ICS:
          -- [OUTPUT] other UTXO:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ partnerToken 1
                ]
          -- [INPUT] Reserve UTXO:
          -- Note: There is no Reserve UTXO output
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ reserveUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ reserveAddress
                            & _txOutDatum .~ V2.OutputDatum (wrapToVersioned reserveDatum)
                            -- carries reserve auth token:
                            & _txOutValue <>~ reserveAuthToken 1
                            -- tokens currently in reserve:
                            & _txOutValue <>~ partnerToken 10
                         )
                ]
      )

-- values

wrapToVersioned :: (ToData a) => a -> V2.Datum
wrapToVersioned datum =
  V2.Datum $
    toBuiltinData $
      Types.VersionedGenericDatum
        { datum
        , genericData = Test.dummyBuiltinData
        , version = 0
        }

reserveDatum :: Types.ReserveDatum
reserveDatum =
  Types.ReserveDatum
    { immutableSettings =
        Types.ImmutableReserveSettings
          { tokenKind = Test.toAsData partnerTokenAssetClass
          }
    , mutableSettings =
        Types.MutableReserveSettings
          { vFunctionTotalAccrued = Test.toAsData vFunctionCurrSym
          , incentiveAmount = 1
          }
    , stats =
        Types.ReserveStats
          { tokenTotalAmountTransferred = 10
          }
    }

initialReserveDatum :: Types.ReserveDatum
initialReserveDatum =
  reserveDatum
    { Types.stats = Types.ReserveStats {tokenTotalAmountTransferred = 0}
    }

partnerToken :: Integer -> V2.Value
partnerToken = V2.singleton tokenCurrSym tokenName
  where
    Value.AssetClass (tokenCurrSym, tokenName) = partnerTokenAssetClass

partnerTokenAssetClass :: Value.AssetClass
partnerTokenAssetClass = Value.AssetClass (tokenCurrSym, tokenName)
  where
    tokenCurrSym = V2.CurrencySymbol "partnerCoinPolicyHash"
    tokenName = V2.TokenName "#PARTNER-COIN"

reserveUtxo :: V2.TxOutRef
reserveUtxo = V2.TxOutRef "77777777" 0

reserveAddress :: V2.Address
reserveAddress = Address.scriptHashAddress reserveValidatorScriptHash

reserveValidatorVersionOracleUtxo :: V2.TxOut
reserveValidatorVersionOracleUtxo =
  Test.mkVersionOracleTxOut
    ScriptId.ReserveValidator
    reserveValidatorScriptHash

reserveValidatorScriptHash :: V2.ScriptHash
reserveValidatorScriptHash = V2.ScriptHash "reserveValidatorScriptHash"

icsUtxo :: V2.TxOutRef
icsUtxo = V2.TxOutRef "aaaaaaaa" 0

icsAddress :: V2.Address
icsAddress = Address.scriptHashAddress icsSupplyValidatorScriptHash

someUtxo :: V2.TxOutRef
someUtxo = V2.TxOutRef "99999999" 99

someAddress :: V2.Address
someAddress = V2.Address (V2.PubKeyCredential "99999999999999999999999999999999999999999999999999999999") Nothing

someAssetClass :: Value.AssetClass
someAssetClass = Value.AssetClass (tokenCurrSym, tokenName)
  where
    tokenCurrSym = V2.CurrencySymbol "somePolicyHash"
    tokenName = V2.TokenName "#SOME"

reserveAuthPolicyVersionOracleUtxo :: V2.TxOut
reserveAuthPolicyVersionOracleUtxo =
  Test.mkVersionOracleTxOut
    ScriptId.ReserveAuthPolicy
    reserveAuthPolicyScriptHash

reserveAuthToken :: Integer -> V2.Value
reserveAuthToken = V2.singleton reserveAuthPolicyCurrencySymbol V2.adaToken

reserveAuthPolicyCurrencySymbol :: V2.CurrencySymbol
reserveAuthPolicyCurrencySymbol = V2.CurrencySymbol . V2.getScriptHash $ reserveAuthPolicyScriptHash

reserveAuthPolicyScriptHash :: V2.ScriptHash
reserveAuthPolicyScriptHash = V2.ScriptHash "reserveAuthPolicyScriptHash"

icsSupplyValidatorVersionOracleUtxo :: V2.TxOut
icsSupplyValidatorVersionOracleUtxo =
  Test.mkVersionOracleTxOut
    ScriptId.IlliquidCirculationSupplyValidator
    icsSupplyValidatorScriptHash

icsSupplyValidatorScriptHash :: V2.ScriptHash
icsSupplyValidatorScriptHash = V2.ScriptHash "icsSupplyValidatorScriptHash"

vFunctionToken :: Integer -> V2.Value
vFunctionToken = V2.singleton vFunctionCurrSym V2.adaToken

vFunctionCurrSym :: V2.CurrencySymbol
vFunctionCurrSym = V2.CurrencySymbol "vFunctionScriptHash"

-- test runner

runMintingPolicy :: Types.VersionOracleConfig -> BuiltinData -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy vc redeemer ctx =
  compiledReserveAuthPolicy
    `appArg` vc
    `appArg` redeemer
    `appArg` ctx

runValidator :: Types.VersionOracleConfig -> BuiltinData -> Types.ReserveRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator vc datum redeemer ctx =
  compiledValidator
    `appArg` vc
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
