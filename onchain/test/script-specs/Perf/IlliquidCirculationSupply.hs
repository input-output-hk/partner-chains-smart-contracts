module Perf.IlliquidCirculationSupply (execCosts) where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..), emptyByteString)
import ScriptUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.IlliquidCirculationSupply
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types as Types

execCosts :: TestTree
execCosts =
  testGroup
    "ICS scripts ExBudget"
    [ goldenPerf "icsAuthorityTokenPolicy" icsAuthorityTokenPolicy
    , goldenPerf "icsValidatorDeposit" icsValidatorDeposit
    , goldenPerf "icsValidatorWithdraw" icsValidatorWithdraw
    ]

icsAuthorityTokenPolicy :: CompiledCode BuiltinUnit
icsAuthorityTokenPolicy =
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

icsValidatorDeposit :: CompiledCode BuiltinUnit
icsValidatorDeposit =
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

icsValidatorWithdraw :: CompiledCode BuiltinUnit
icsValidatorWithdraw =
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

-- values

supplyUtxo :: V2.TxOutRef
supplyUtxo = V2.TxOutRef "01234abc" 0

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
