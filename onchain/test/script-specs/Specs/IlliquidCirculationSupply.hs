module Specs.IlliquidCirculationSupply (
  policyTests,
  validatorTests,
) where

import ApiBuilder
import Control.Lens
import PartnerChains.ScriptId qualified as ScriptId
import PartnerChains.Scripts.IlliquidCirculationSupply
import PartnerChains.Types as Types
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..), emptyByteString)
import Test.Tasty
import TestValues qualified as Test
import Testing

-- minting policy

policyTests :: TestTree
policyTests =
  testGroup
    "illiquid circulation supply policy"
    [ testGroup
        "authority token policy"
        [ icsAuthorityTokenPolicyPassing
        , icsAuthorityTokenPolicyFailing01
        ]
    ]

icsAuthorityTokenPolicyPassing :: TestTree
icsAuthorityTokenPolicyPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.versionOracleConfig
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- minted tokens are all sent to dParameterValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken 1
      )

icsAuthorityTokenPolicyFailing01 :: TestTree
icsAuthorityTokenPolicyFailing01 =
  expectFail "should fail if not signed by the governance authority" "ERROR-ICS-AUTH-TOKEN-01" $
    runMintingPolicy
      Test.versionOracleConfig
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          -- & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- minted tokens are all sent to dParameterValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken 2
      )

-- validator

validatorTests :: TestTree
validatorTests =
  testGroup
    "illiquid circulation supply validator"
    [ testGroup
        "deposit redeemer"
        [ icsValidatorDepositPassing
        , icsValidatorDepositFailing01a
        , icsValidatorDepositFailing01b
        , icsValidatorDepositFailing01c
        , icsValidatorDepositFailing02
        , icsValidatorDepositFailing04
        ]
    , testGroup
        "withdraw redeemer"
        [ icsValidatorWithdrawPassing
        , icsValidatorWithdrawFailing01a
        , icsValidatorWithdrawFailing01b
        , icsValidatorWithdrawFailing03
        , icsValidatorWithdrawFailing05
        ]
    ]

-- deposit redeemer

icsValidatorDepositPassing :: TestTree
icsValidatorDepositPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 5
                    & _txOutValue <>~ icsAuthorityToken 1
                ]
      )

icsValidatorDepositFailing01a :: TestTree
icsValidatorDepositFailing01a =
  expectFail "should fail if output UTxO has 0 ICS Authority Tokens" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 5
                    -- output utxo has 0 ICS Authority Tokens:
                    & _txOutValue <>~ icsAuthorityToken 0
                ]
      )

icsValidatorDepositFailing01b :: TestTree
icsValidatorDepositFailing01b =
  expectFail "should fail if output UTxO has 2 ICS Authority Tokens" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 5
                    -- output utxo has 2 ICS Authority Tokens:
                    & _txOutValue <>~ icsAuthorityToken 2
                ]
      )

icsValidatorDepositFailing01c :: TestTree
icsValidatorDepositFailing01c =
  expectFail "should fail if ICS auth tokens leak from the ICS validator" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 5
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                ]
      )

icsValidatorDepositFailing02 :: TestTree
icsValidatorDepositFailing02 =
  expectFail "should fail if assets of the supply UTxO decreased" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-02" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    -- supply utxo asset decreased from 3 to 2:
                    & _txOutValue <>~ supplyToken 2
                    & _txOutValue <>~ icsAuthorityToken 1
                ]
      )

icsValidatorDepositFailing04 :: TestTree
icsValidatorDepositFailing04 =
  expectFail "should fail if no unique output UTxO at the supply address" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-04" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 5
                    & _txOutValue <>~ icsAuthorityToken 1
                ]
          -- another output at supply address:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 4
                    & _txOutValue <>~ icsAuthorityToken 1
                ]
      )

-- withdraw redeemer

icsValidatorWithdrawPassing :: TestTree
icsValidatorWithdrawPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ supplyToken 3
                ]
          & _scriptContextTxInfo . _txInfoMint <>~ icsWithdrawalToken 1
      )

icsValidatorWithdrawFailing03 :: TestTree
icsValidatorWithdrawFailing03 =
  expectFail "should fail if single illiquid circulation supply token is not minted" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-03" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ supplyToken 3
                ]
      )

icsValidatorWithdrawFailing01a :: TestTree
icsValidatorWithdrawFailing01a =
  expectFail "should fail if some ICS output does not have exactly one ICS Auth token" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ supplyToken 2
                , emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ supplyToken 1
                ]
          & _scriptContextTxInfo . _txInfoMint <>~ icsWithdrawalToken 1
      )

icsValidatorWithdrawFailing01b :: TestTree
icsValidatorWithdrawFailing01b =
  expectFail "should fail if ICS auth tokens leak from the ICS validator" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-01" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                , emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo2
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 5
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ supplyToken 3
                ]
          & _scriptContextTxInfo . _txInfoMint <>~ icsWithdrawalToken 1
      )

icsValidatorWithdrawFailing05 :: TestTree
icsValidatorWithdrawFailing05 =
  expectFail "should fail if no own input UTxO at the supply address" "ERROR-ILLIQUID-CIRCULATION-SUPPLY-05" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          & _scriptContextTxInfo . _txInfoReferenceInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsAuthorityTokenUtxo]
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    -- [ERROR] missing supplyUtxo from inputs
                    & _txInInfoOutRef .~ otherUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutValue <>~ supplyToken 3
                            & _txOutValue <>~ icsAuthorityToken 1
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutValue <>~ icsAuthorityToken 1
                , emptyTxOut
                    & _txOutAddress .~ someAddress
                    & _txOutValue <>~ supplyToken 3
                ]
          & _scriptContextTxInfo . _txInfoMint <>~ icsWithdrawalToken 1
      )

-- values

supplyUtxo :: V2.TxOutRef
supplyUtxo = V2.TxOutRef "01234abc" 0

supplyUtxo2 :: V2.TxOutRef
supplyUtxo2 = V2.TxOutRef "01234ab2" 0

otherUtxo :: V2.TxOutRef
otherUtxo = V2.TxOutRef "56789abc" 1

someAddress :: V2.Address
someAddress = V2.Address (V2.PubKeyCredential "05550555055505550555055505550555055505550555055505550555") Nothing

supplyAddress :: V2.Address
supplyAddress = V2.Address (V2.PubKeyCredential "06660666066606660666066606660666066606660666066606660666") Nothing

supplyToken :: Integer -> V2.Value
supplyToken = V2.singleton supplyTokenCurrSym supplyTokenName

supplyTokenName :: V2.TokenName
supplyTokenName = V2.TokenName "supplyToken"

supplyTokenCurrSym :: V2.CurrencySymbol
supplyTokenCurrSym = V2.CurrencySymbol . V2.getScriptHash $ supplyTokenValidatorScriptHash

supplyTokenValidatorScriptHash :: V2.ScriptHash
supplyTokenValidatorScriptHash = V2.ScriptHash "supplyTokenValidatorScriptHash"

icsWithdrawalTokenCurrSym :: V2.CurrencySymbol
icsWithdrawalTokenCurrSym = V2.CurrencySymbol . V2.getScriptHash $ icsWithdrawalTokenValidatorScriptHash

icsWithdrawalTokenValidatorScriptHash :: V2.ScriptHash
icsWithdrawalTokenValidatorScriptHash = V2.ScriptHash "icsWithdrawalTokenValidatorScriptHash"

icsTokenUtxo :: V2.TxOut
icsTokenUtxo =
  mkTxOut
    Test.versionValidatorAddress
    (Test.versionOracleToken 1)
    icsVersionOracleDatum
    icsWithdrawalTokenValidatorScriptHash
  where
    icsVersionOracleDatum :: Types.VersionOracleDatum
    icsVersionOracleDatum =
      Types.VersionOracleDatum
        Types.VersionOracle {scriptId = ScriptId.illiquidCirculationSupplyWithdrawalPolicyId}
        (Test.toAsData Test.versioningCurrSym)

icsAuthorityTokenUtxo :: V2.TxOut
icsAuthorityTokenUtxo =
  mkTxOut
    Test.versionValidatorAddress
    (Test.versionOracleToken 1)
    icsVersionOracleDatum
    icsAuthorityTokenScriptHash
  where
    icsVersionOracleDatum :: Types.VersionOracleDatum
    icsVersionOracleDatum =
      Types.VersionOracleDatum
        Types.VersionOracle {scriptId = ScriptId.illiquidCirculationSupplyAuthorityTokenPolicyId}
        (Test.toAsData Test.versioningCurrSym)

icsAuthorityTokenScriptHash :: V2.ScriptHash
icsAuthorityTokenScriptHash = V2.ScriptHash "icsAuthorityTokenScriptHash"

icsAuthorityTokenCurrSym :: V2.CurrencySymbol
icsAuthorityTokenCurrSym = V2.CurrencySymbol . V2.getScriptHash $ icsAuthorityTokenScriptHash

icsAuthorityToken :: Integer -> V2.Value
icsAuthorityToken = V2.singleton icsAuthorityTokenCurrSym icsAuthorityTokenTokenName

icsAuthorityTokenTokenName :: V2.TokenName
icsAuthorityTokenTokenName = V2.TokenName emptyByteString

-- this is fixed in the script
icsWithdrawalTokenName :: V2.TokenName
icsWithdrawalTokenName = V2.TokenName emptyByteString

icsWithdrawalToken :: Integer -> V2.Value
icsWithdrawalToken = V2.singleton icsWithdrawalTokenCurrSym icsWithdrawalTokenName

-- test runner

runMintingPolicy :: VersionOracleConfig -> V2.ScriptContext -> CompiledCode BuiltinUnit
runMintingPolicy versionOracleConfig ctx =
  compiledAuthorityTokenPolicy
    `appArg` Test.dummyBuiltinData
    `appArg` versionOracleConfig
    `appArg` Test.dummyBuiltinData
    `appArg` ctx

runValidator :: VersionOracleConfig -> BuiltinData -> IlliquidCirculationSupplyRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator versionOracleConfig datum redeemer ctx =
  compiledValidator
    `appArg` versionOracleConfig
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
