module IlliquidCirculationSupply where

import Control.Lens
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..), emptyByteString)
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.IlliquidCirculationSupply
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types as Types

-- minting policy

policyTests :: TestTree
policyTests =
  testGroup
    "illiquid circulation supply policy"
    [ testGroup
        "authority token policy"
        [ illiquidCirculationSupplyAuthorityTokenPolicyPassing
        , illiquidCirculationSupplyAuthorityTokenPolicyFailing01
        ]
    ]

illiquidCirculationSupplyAuthorityTokenPolicyPassing :: TestTree
illiquidCirculationSupplyAuthorityTokenPolicyPassing =
  expectSuccess "should pass" $
    runPolicy
      Test.versionOracleConfig
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          -- signed by governance:
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          -- minted tokens are all sent to dParameterValidatorAddress:
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken 2
      )

illiquidCirculationSupplyAuthorityTokenPolicyFailing01 :: TestTree
illiquidCirculationSupplyAuthorityTokenPolicyFailing01 =
  expectFail "should fail if not signed by the governance authority (ERROR-ICS-AUTH-TOKEN-01)" $
    runPolicy
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
        [ illiquidCirculationSupplyValidatorDepositPassing
        , illiquidCirculationSupplyValidatorDepositFailing01a
        , illiquidCirculationSupplyValidatorDepositFailing01b
        , illiquidCirculationSupplyValidatorDepositFailing02
        , illiquidCirculationSupplyValidatorDepositFailing03
        , illiquidCirculationSupplyValidatorDepositFailing05
        ]
    , testGroup
        "withdraw redeemer"
        [ illiquidCirculationSupplyValidatorWithdrawPassing
        , illiquidCirculationSupplyValidatorWithdrawFailing04
        ]
    ]

-- deposit redeemer

illiquidCirculationSupplyValidatorDepositPassing :: TestTree
illiquidCirculationSupplyValidatorDepositPassing =
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

illiquidCirculationSupplyValidatorDepositFailing01a :: TestTree
illiquidCirculationSupplyValidatorDepositFailing01a =
  expectFail "should fail if output UTxO has 0 ICS Authority Tokens (ERROR-ILLIQUID-CIRCULATION-SUPPLY-01)" $
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

illiquidCirculationSupplyValidatorDepositFailing01b :: TestTree
illiquidCirculationSupplyValidatorDepositFailing01b =
  expectFail "should fail if output UTxO has 2 ICS Authority Tokens (ERROR-ILLIQUID-CIRCULATION-SUPPLY-01)" $
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

illiquidCirculationSupplyValidatorDepositFailing02 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing02 =
  expectFail "should fail if assets of the supply UTxO decreased (ERROR-ILLIQUID-CIRCULATION-SUPPLY-02)" $
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

illiquidCirculationSupplyValidatorDepositFailing03 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing03 =
  expectFail "should fail if ics tokens leak from the ICS validator (ERROR-ILLIQUID-CIRCULATION-SUPPLY-03)" $
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
  where
    someAddress :: V2.Address
    someAddress = V2.Address (V2.PubKeyCredential "05550555055505550555055505550555055505550555055505550555") Nothing

illiquidCirculationSupplyValidatorDepositFailing05 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing05 =
  expectFail "should fail if no unique output UTxO at the supply address (ERROR-ILLIQUID-CIRCULATION-SUPPLY-05)" $
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

illiquidCirculationSupplyValidatorWithdrawPassing :: TestTree
illiquidCirculationSupplyValidatorWithdrawPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsWithdrawalTokenCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ icsWithdrawalToken 1
      )

illiquidCirculationSupplyValidatorWithdrawFailing04 :: TestTree
illiquidCirculationSupplyValidatorWithdrawFailing04 =
  expectFail "should fail if single illiquid circulation supply token is not minted (ERROR-ILLIQUID-CIRCULATION-SUPPLY-04)" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      WithdrawFromSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsWithdrawalTokenCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ icsTokenUtxo]
          -- withdrawal token minted != 0
          & _scriptContextTxInfo . _txInfoMint <>~ icsWithdrawalToken 2
      )

-- values

dummyDatum :: V2.Datum
dummyDatum = V2.Datum $ toBuiltinData (0 :: Integer)

supplyUtxo :: V2.TxOutRef
supplyUtxo = V2.TxOutRef "01234abc" 0

otherUtxo :: V2.TxOutRef
otherUtxo = V2.TxOutRef "56789abc" 1

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

runValidator :: VersionOracleConfig -> BuiltinData -> IlliquidCirculationSupplyRedeemer -> V2.ScriptContext -> BuiltinUnit
runValidator versionOracleConfig datum redeemer ctx =
  mkIlliquidCirculationSupplyValidatorUntyped
    (toBuiltinData versionOracleConfig)
    (toBuiltinData (V1.AssetClass (supplyTokenCurrSym, supplyTokenName)))
    (toBuiltinData datum)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)

runPolicy :: VersionOracleConfig -> V2.ScriptContext -> BuiltinUnit
runPolicy versionOracleConfig ctx =
  mkIlliquidCirculationSupplyAuthorityTokenPolicyUntyped
    Test.dummyBuiltinData
    (toBuiltinData versionOracleConfig)
    Test.dummyBuiltinData
    (toBuiltinData ctx)
