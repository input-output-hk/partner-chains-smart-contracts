module IlliquidCirculationSupply where

import Control.Lens
import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..), emptyByteString)
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.IlliquidCirculationSupply
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types as Types
import Prelude

-- validator

validatorTests :: TestTree
validatorTests =
  testGroup
    "illiquid circulation supply validator"
    [ testGroup
        "deposit redeemer"
        [ illiquidCirculationSupplyValidatorDepositPassing
        , illiquidCirculationSupplyValidatorDepositFailing01
        , illiquidCirculationSupplyValidatorDepositFailing02
        , illiquidCirculationSupplyValidatorDepositFailing03
        , illiquidCirculationSupplyValidatorDepositFailing05
        , illiquidCirculationSupplyValidatorDepositFailing06
        ]
    , testGroup
        "withdraw redeemer"
        [ illiquidCirculationSupplyValidatorWithdrawPassing
        , illiquidCirculationSupplyValidatorWithdrawFailing04
        ]
    ]

illiquidCirculationSupplyValidatorDepositPassing :: TestTree
illiquidCirculationSupplyValidatorDepositPassing =
  expectSuccess "should pass" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                            & _txOutValue <>~ supplyToken 3
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                    & _txOutValue <>~ supplyToken 5
                ]
      )

illiquidCirculationSupplyValidatorDepositFailing01 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing01 =
  expectFail "should fail if input UTxO has non-unit datum (ERROR-ILLIQUID-CIRCULATION-SUPPLY-01)" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            -- input utxo has non-unit datum:
                            & _txOutDatum .~ V2.OutputDatum dummyDatum
                            & _txOutValue <>~ supplyToken 3
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                    & _txOutValue <>~ supplyToken 5
                ]
      )

illiquidCirculationSupplyValidatorDepositFailing02 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing02 =
  expectFail "should fail if output UTxO has non-unit datum (ERROR-ILLIQUID-CIRCULATION-SUPPLY-02)" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                            & _txOutValue <>~ supplyToken 3
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    -- output utxo has non-unit datum:
                    & _txOutDatum .~ V2.OutputDatum dummyDatum
                    & _txOutValue <>~ supplyToken 5
                ]
      )

illiquidCirculationSupplyValidatorDepositFailing03 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing03 =
  expectFail "should fail if assets of the supply UTxO decreased (ERROR-ILLIQUID-CIRCULATION-SUPPLY-03)" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                            & _txOutValue <>~ supplyToken 3
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                    -- supply utxo asset decreased from 3 to 2:
                    & _txOutValue <>~ supplyToken 2
                ]
      )

illiquidCirculationSupplyValidatorDepositFailing05 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing05 =
  expectFail "should fail if no unique input UTxO at the supply address (ERROR-ILLIQUID-CIRCULATION-SUPPLY-05)" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                            & _txOutValue <>~ supplyToken 3
                         )
                ]
          -- another input utxo at supply address:
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ otherUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                            & _txOutValue <>~ supplyToken 9
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                    & _txOutValue <>~ supplyToken 5
                ]
      )

illiquidCirculationSupplyValidatorDepositFailing06 :: TestTree
illiquidCirculationSupplyValidatorDepositFailing06 =
  expectFail "should fail if no unique output UTxO at the supply address (ERROR-ILLIQUID-CIRCULATION-SUPPLY-06)" $
    runValidator
      Test.versionOracleConfig
      Test.dummyBuiltinData
      DepositMoreToSupply
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending supplyUtxo
          & _scriptContextTxInfo . _txInfoInputs
            <>~ [ emptyTxInInfo
                    & _txInInfoOutRef .~ supplyUtxo
                    & _txInInfoResolved
                      .~ ( emptyTxOut
                            & _txOutAddress .~ supplyAddress
                            & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                            & _txOutValue <>~ supplyToken 3
                         )
                ]
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                    & _txOutValue <>~ supplyToken 5
                ]
          -- another output at supply address:
          & _scriptContextTxInfo . _txInfoOutputs
            <>~ [ emptyTxOut
                    & _txOutAddress .~ supplyAddress
                    & _txOutDatum .~ V2.OutputDatum versionedUnitDatum
                    & _txOutValue <>~ supplyToken 4
                ]
      )

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

versionedUnitDatum :: V2.Datum
versionedUnitDatum =
  V2.Datum $
    toBuiltinData $
      VersionedGenericDatum
        { datum = ()
        , genericData = toBuiltinData ()
        , version = 0
        }

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
  where
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
    Test.versionOracleToken
    icsVersionOracleDatum
    icsWithdrawalTokenValidatorScriptHash
  where
    icsVersionOracleDatum :: Types.VersionOracleDatum
    icsVersionOracleDatum =
      Types.VersionOracleDatum
        Types.VersionOracle {scriptId = ScriptId.illiquidCirculationSupplyWithdrawalPolicyId}
        (Test.toAsData Test.versioningCurrSym)

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
    (toBuiltinData datum)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)
