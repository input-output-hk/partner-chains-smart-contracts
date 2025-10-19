module Perf.Reserve (execCosts) where

import Prelude

import ApiBuilder
import Control.Lens
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as C8
import PartnerChains.ScriptId qualified as ScriptId
import PartnerChains.Scripts.Reserve
import PartnerChains.Types qualified as Types
import PlutusLedgerApi.Common (MajorProtocolVersion (..), PlutusLedgerLanguage (..), ScriptNamedDeBruijn (..), deserialiseScript, deserialisedScript)
import PlutusLedgerApi.V1.Address qualified as Address
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import PlutusTx.Code (CompiledCodeIn (..))
import Test.Tasty
import TestValues qualified as Test
import Testing

import Data.ByteString.Short qualified as BSS

execCosts :: TestTree
execCosts =
  testGroup
    "Reserve scripts ExBudget"
    [ goldenPerf "reserveAuthPolicy" reserveAuthPolicy
    , goldenPerf "reserveValidatorDeposit" reserveValidatorDeposit
    , goldenPerf "reserveValidatorUpdate" reserveValidatorUpdate
    , goldenPerf "reserveValidatorTransferToICS" reserveValidatorTransferToICS
    , goldenPerf "aikenReserveValidatorTransferToICS" aikenReserveValidatorTransferToICS
    , goldenPerf "reserveValidatorHandover" reserveValidatorHandover
    ]

reserveAuthPolicy :: CompiledCode BuiltinUnit
reserveAuthPolicy =
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

reserveValidatorDeposit :: CompiledCode BuiltinUnit
reserveValidatorDeposit =
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

reserveValidatorUpdate :: CompiledCode BuiltinUnit
reserveValidatorUpdate =
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

reserveValidatorTransferToICS :: CompiledCode BuiltinUnit
reserveValidatorTransferToICS =
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

aikenReserveValidatorTransferToICS :: CompiledCode BuiltinUnit
aikenReserveValidatorTransferToICS =
  runAikenValidator
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

reserveValidatorHandover :: CompiledCode BuiltinUnit
reserveValidatorHandover =
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

-- values

wrapToVersioned :: (ToData a) => a -> V2.Datum
wrapToVersioned datum =
  V2.Datum $
    toBuiltinData $
      Types.VersionedGenericDatum
        { datum
        , appendix = Test.dummyBuiltinData
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

runAikenValidator :: Types.VersionOracleConfig -> BuiltinData -> Types.ReserveRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
runAikenValidator vc datum redeemer ctx =
  aikenCompiledValidator
    `appArg` vc
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx

-- aikenValidator :: String
-- aikenValidator = "591872010100229800aba4aba2aba1aba0aab9faab9eaab9dab9cab9a9bae0024888888888a60022a660069201ff657870656374205b6f75747075745f6963735f7574786f5d203d0a20202020202073656c662e6f7574707574730a20202020202020207c3e206c6973742e66696c746572280a202020202020202020202020666e286f757470757429207b0a20202020202020202020202020206c65742061646472203d206f75747075742e616464726573730a2020202020202020202020202020616464722e7061796d656e745f63726564656e7469616c203d3d20536372697074280a20202020202020202020202020202020696c6c69717569645f63697263756c6174696f6e5f737570706c795f7363726970745f686173682c0a20202020202020202020202020201c290a2020202020202020202020207d2c0a202020202020202020202900168a99801a493865787065637420726573657276655f6f75747075745f646174756d3a2052657365727665446174756d203d206f75747075745f646174756d00168a99801a493665787065637420726573657276655f696e7075745f646174756d3a2052657365727665446174756d203d20696e7075745f646174756d00168a99801a4969657870656374205b6f75747075745f7574786f5d203d0a20202020202073656c662e6f7574707574730a20202020202020207c3e207472616e73616374696f6e2e66696e645f7363726970745f6f75747075747328726573657276655f7363726970745f686173682900168a99801a49ff657870656374205b696c6c697175696443697263756c6174696f6e537570706c795265666572656e6365496e7075745d203d0a20202020202073656c662e7265666572656e63655f696e707574730a20202020202020207c3e206c6973742e66696c746572280a202020202020202020202020666e28696e70757429207b0a20202020202020202020202020206c6574206f7574707574203d20696e7075742e6f75747075740a20202020202020202020202020206c6574206861735f746f6b656e203d0a202020202020202020202020202020206173736574732e746f5f64696374286f75747075742e76616c7565290a20202020202020202020202020ff20202020207c3e20646963742e6765742876657273696f6e5f6f7261636c655f636f6e666967290a2020202020202020202020202020202020207c3e206f7074696f6e2e616e645f7468656e28646963742e676574285f2c202256657273696f6e206f7261636c652229290a2020202020202020202020202020202020207c3e206f7074696f6e2e69735f736f6d6528290a0a20202020202020202020202020206966206f75747075742e646174756d20697320496e6c696e65446174756d2864617461293a20446174756d207b0a202020202020202020202020202020206966206461746120697320646174756d3a2056657273696f6e696e6744617475d26d207b0a2020202020202020202020202020202020206861735f746f6b656e20262620646174756d2e7363726970744964203d3d2033300a202020202020202020202020202020207d20656c7365207b0a20202020202020202020202020202020202046616c73650a202020202020202020202020202020207d0a20202020202020202020202020207d20656c7365207b0a2020202020202020202020202020202046616c73650a20202020202020202020202020207d0a2020202020202020202020207d2c0a202020202020202020202900168a99801a49ff657870656374205b7265736572766541757468546f6b656e5265666572656e6365496e7075745d203d0a20202020202073656c662e7265666572656e63655f696e707574730a20202020202020207c3e206c6973742e66696c746572280a202020202020202020202020666e28696e70757429207b0a20202020202020202020202020206c6574206f7574707574203d20696e7075742e6f75747075740a20202020202020202020202020206c6574206861735f746f6b656e203d0a202020202020202020202020202020206173736574732e746f5f64696374286f75747075742e76616c7565290a2020202020202020202020202020202020207c3e2064ff6963742e6765742876657273696f6e5f6f7261636c655f636f6e666967290a2020202020202020202020202020202020207c3e206f7074696f6e2e616e645f7468656e28646963742e676574285f2c202256657273696f6e206f7261636c652229290a2020202020202020202020202020202020207c3e206f7074696f6e2e69735f736f6d6528290a0a20202020202020202020202020206966206f75747075742e646174756d20697320496e6c696e65446174756d2864617461293a20446174756d207b0a202020202020202020202020202020206966206461746120697320646174756d3a2056657273696f6e696e67446174756d207b0a2020202020c9202020202020202020202020206861735f746f6b656e20262620646174756d2e7363726970744964203d3d2032390a202020202020202020202020202020207d20656c7365207b0a20202020202020202020202020202020202046616c73650a202020202020202020202020202020207d0a20202020202020202020202020207d20656c7365207b0a2020202020202020202020202020202046616c73650a20202020202020202020202020207d0a2020202020202020202020207d2c0a202020202020202020202900168a99801a49ff657870656374205b676f7665726e616e63655265666572656e6365496e7075745d203d0a20202020202073656c662e7265666572656e63655f696e707574730a20202020202020207c3e206c6973742e66696c746572280a202020202020202020202020666e28696e70757429207b0a20202020202020202020202020206c6574206f7574707574203d20696e7075742e6f75747075740a20202020202020202020202020206c6574206861735f746f6b656e203d0a202020202020202020202020202020206173736574732e746f5f64696374286f75747075742e76616c7565290a2020202020202020202020202020202020207c3e20646963742e6765ff742876657273696f6e5f6f7261636c655f636f6e666967290a2020202020202020202020202020202020207c3e206f7074696f6e2e616e645f7468656e28646963742e676574285f2c202256657273696f6e206f7261636c652229290a2020202020202020202020202020202020207c3e206f7074696f6e2e69735f736f6d6528290a0a20202020202020202020202020206966206f75747075742e646174756d20697320496e6c696e65446174756d2864617461293a20446174756d207b0a202020202020202020202020202020206966206461746120697320646174756d3a2056657273696f6e696e67446174756d207b0a2020202020202020202020c3202020202020206861735f746f6b656e20262620646174756d2e7363726970744964203d3d2033320a202020202020202020202020202020207d20656c7365207b0a20202020202020202020202020202020202046616c73650a202020202020202020202020202020207d0a20202020202020202020202020207d20656c7365207b0a2020202020202020202020202020202046616c73650a20202020202020202020202020207d0a2020202020202020202020207d2c0a202020202020202020202900168a99801a493365787065637420726573657276655f72656465656d65723a205265736572766552656465656d6572203d2072656465656d657200164888888896600264653001301200198091809800cdc3a400530120024888966002600460246ea800e2646644a6602892010648454c4c4f3300159800803c4cc88cc89660026006003159800980d1baa00b801403901b45660026012003159800980d1baa00b801403901b45660026008003159800980d1baa00b801403901b456600266e1d20060018acc004c068dd5005c00a01c80da01c80b9017202e405c3300122259800801c006264b3001001801400a0051332259800800c012264b3001001802c01600b0058992cc004c09000e00f00640846eb80050241810800a03e375a00260400090024084603c00680e246038603a603a0032598009801180c1baa0018a518a504059374a900048966002601260326ea800a298103d87a8000898009bab301d301a375400480ba44646600200200644b30010018a5eb8226644b3001300500289981000119802002000c4cc01001000501b180f8009810000a03a9180e180e800a4444444664464b300100180bc4c966002604e00513259800800c06a264b300130290028992cc00400603b13259800981580146600244b3001301830283754005100189bae302c302937540048132460566058605860580032302b302c302c302c302c001912cc004c060c0a0dd50014400626eb4c0b0c0a4dd5001204c99198008009bac302b3028375402c44b30010018a60103d87a80008992cc004cdd7981698151baa0010168980799816000a5eb82266006006605c0048138c0b000502a2444453001330053004302c3754601c60586ea80194cc0a924010e6d697373696e67207363726970740016acc004c054c0acdd5198061bab3003302c37540346600a600860586ea8c038c0b0dd500529981524810e6d697373696e672073637269707400168a518a5040a53370e90009980119808198061bab3003302c37540346600a600860586ea8c038c0b0dd500429981524810e6d697373696e6720736372697074001623300e001489004800122259800980c18171baa004899192cc004c080c0c0dd5000c4c96600200302a8992cc004c0dc00a26644b3001301f3034375400313259800800c0be05f1332298008014660026eb8c0ecc0e0dd5181d8014dd7180d181c1baa303b002980f981c1baa006488966002604a60746ea8006264b300100181b40da26530010018cc004cc04ccc084cc074dd5980f981e9baa00e00523301f0010054800266026660426603a6eacc07cc0f4dd500580291980f800802a40015980099baf30400013040007899b87375a60480026eb4c09001e294103a56600266ebcc100004c10001e266ebcc07c004c07c01e294103a4cc080dd61812181e9baa02b23375e6082607c6ea8c104c0f8dd500099ba548008cc100dd480925eb8122222332259800801c0fe264b300130490048cc004cc06ccc0a4cc094dd5981398229baa00100d23302700100d480026466002002660526eb0c124c118dd501a119baf304a304737546094608e6ea8c0a4c11cdd500099ba548008cc124dd480da5eb808966002003148002266e00cc008008c12c004cc074cc0accc09cdd5981498239baa302930473754609400201e46605200201e900020909980d99814998129bab301c304537540666eb8c120dd61813807919813800a441004800260886ea80d92222598009819000c566002b300101c8a518a99823a481176d696e7473476f7665726e616e6365203f2046616c73650014a082322b30015980080b4528c54cc11d24011d646174756d5f646f65735f6e6f745f6368616e6765203f2046616c73650014a082322b30010078a518a99823a48124726573657276655f746f6b656e735f6f6e6c795f696e637265617365203f2046616c73650014a0823229410464528208c8acc004c0e00062b300159800804c528c54cc11d24122646174756d5f6368616e67655f6f6e6c795f62795f7374617473203f2046616c73650014a082322b30015980099b87337026eb4c0c004c008cdc08058064528c54cc11d2412661737365745f6368616e67655f62795f636f72726563745f616d6f756e74203f2046616c73650014a082322b30013370e66e00010dd698159bac302b0133370066e04008dd69818009801c528c54cc11d24129636f72726563745f616d6f756e745f7472616e736665727265645f746f5f696373203f2046616c73650014a0823229410464528208c8acc004c0cc0062b30015980080e4528c54cc11d241176d696e7473476f7665726e616e6365203f2046616c73650014a082322b3001598008054528c54cc11d24012d646174756d5f6368616e67655f6f6e6c795f62795f6d757461626c655f73657474696e6773203f2046616c73650014a082322b30010068a518a99823a4811c6173736574735f646f5f6e6f745f6368616e6765203f2046616c73650014a0823229410464528208c8acc0056600203914a31533047491176d696e7473476f7665726e616e6365203f2046616c73650014a082322b30015980080dc528c54cc11d24011a6275726e735f726573657276655f61757468203f2046616c73650014a082322b30013370e66e0000c03001229462a6608e92012c616c6c5f726573657276655f746f6b656e735f7472616e7366657265645f746f5f696373203f2046616c73650014a0823229410464528208c4118823104620808230c11c00d04519b890050043370e00800a81ba00c375800303681b2082303e303b3754003153303949013465787065637420496e6c696e65446174756d286f75747075745f646174756d29203d206f75747075745f7574786f2e646174756d001640e0818a0023758002444b3001003800c4c96600200313259800800c00e264b3001001802401226644b300100180344c966002003007803c01e264b300130440038acc004c0a4c0fcdd5003c4c9660020030098992cc00400601500a805402a26644b300100180644c96600200300d806c03601b132598009825001c56600201500e8992cc00400601f00f807c03e26644b3001001808c4c966002003012809404a264b3001304f00380a404d04c1bad001809209e304c00141286eb8004c12c02d04c1824805208e807208e375c0028250c11c0050451bae0013046002411c60880028210c100dd5003c02103d40210411bad001803a088304100140fc6eb0004c10000a0090044104607c00281e0c0f8012005002801400903f181e001a074817c0bd03b181c181a9baa0018a99819a4813265787065637420496e6c696e65446174756d28696e7075745f646174756d29203d20696e7075745f7574786f2e646174756d001640c866ebcc068c0ccdd5002180d18199baa001301a3033375400902b40d0606a0028198cc88cc0580088c966002604860686ea8006266e3c00cdd7181c181a9baa0018a5040c8606e60686ea8c0dcc0d0dd50009bac30183031375403e6eb8c0d0c0c4dd5000c54cc0bd24014a6578706563742053637269707428726573657276655f7363726970745f6861736829203d20696e7075745f7574786f2e616464726573732e7061796d656e745f63726564656e7469616c001640b8606660606ea8c0ccc0c0dd5000980898179baa3032302f3754009153302d49014e65787065637420536f6d6528696e70757429203d0a20202020202073656c662e696e707574730a20202020202020207c3e207472616e73616374696f6e2e66696e645f696e707574287574786f29001640b040790281814800a04e330083758600e604a6ea804c8ca6002601a660166600e6eacc024c09cdd5000811919804800a450e56657273696f6e206f7261636c6500980718139baa0018a50488966002602860526ea800a264b300100180144c96600260600051325980080140060031329800800c56600200f13370e6eb4c0c4005203c8a5040ad00240586eb000a00300140c829420068168c0b800502c18151baa002800a04e180418131baa00180da04c302700140946600c6eb0c014c08cdd50089194c004c02ccc024cc014dd5980398129baa0010212330070014890e56657273696f6e206f7261636c6500980618129baa0018a504889660026024604e6ea800a264b300100180144c966002605c0051325980080140060031329800800c56600200f13370e6eb4c0bc005203a8a5040a500240506eb000a00300140c029420068158c0b000502a18141baa002800a04a180318121baa00180c20483025001408c660086eb0c00cc084dd50079194c004c024cc01ccc00cdd5980298119baa00101f2330050014890e56657273696f6e206f7261636c6500980518119baa0018a504889660026020604a6ea800a264b300100180144c96600260580051325980080140060031329800800c56600200f13370e6eb4c0b400520408a50409d00240486eb000a00300140b829420068148c0a800502818131baa002800a046180218111baa0012232330010010032259800800c530103d87a8000899192cc004cdc8802800c56600266e3c014006260146604e604a00497ae08a60103d87a80004089133004004302900340886eb8c08c004c098005024111919800800801912cc0040062980103d87a8000899192cc004cdc8802800c56600266e3c014006260146604e604a00497ae08a60103d87a80004089133004004302900340886eb8c08c004c0980050240c05cdd50049b8748010dc3a400100a805402a01480d8c05c004c05cc060004c04cdd5001c590100c048004c034dd5009c5268a99805a491856616c696461746f722072657475726e65642066616c7365001365640281"

plutusValidator :: String
plutusValidator = "5911b70100003322323232323232323232323332223232332232323233223232323232323232323232323232323232323232323232323232323222225335323232323232323232323235330182302c4901104552524f522d524553455256452d3036000012223232323232323232325323323233350191533533029491104552524f522d524553455256452d3031003302001a01b1533533029491104552524f522d524553455256452d3032003232532335333573466e1d200035573a6ea800c0fc0f854cd4ccd5cd19b8748000d55ce9baa00203f03e103f130014984c00526232532335333573466e1d200235573a004082080264a66a666ae68cdc3a40046aae740041081044ccd5cd19b8f375c6ae84d55cf0019bae357426aae780041081044c00926375400826002931299a999ab9a3370e90021aab9d0020410401325335333573466e1d200435573a0020840822666ae68cdd79aba135573c0066ae84d55cf00082102088209baa004104037540066066a0106064022266052921104552524f522d524553455256452d303300333573466e25400d200003b03c103b103b15335330294901104552524f522d524553455256452d3135003302001a01b1533533029491104552524f522d524553455256452d313600333573466e1d2001333022500933302101a480e806d2210003c03b1330294901104552524f522d524553455256452d313700333573466e1d4008cdc01805008a80081e01d881d881d89919299a99815a49104552524f522d524553455256452d313100333573466e1d4014cdc080080101f01e8a99a99815a481104552524f522d524553455256452d313200333573466ebcc128ccc130d4c03888800c88848ccc00401000c01804003cc12940200f80f44cc0ad2401104552524f522d524553455256452d313300333573466e1ccdc028021a9a98071110019110011100099b8033702004002a00607c07a207a207a6a601a444006444002666044a0126eb8dd49a9a980611100191100111001245001323300448000004c8ccc05480040a8ccc05080048cc0a4ccc0d0d5d09aba235573c6ea80040c80c4008049401c4c020cc0448c0e52401104552524f522d524553455256452d3134003233301320012330283330330010310300023301502e02d50061533533027491104552524f522d524553455256452d3038003301e018019132533533028491104552524f522d524553455256452d303900333573466ebcc11cccc124d4c02c88800c888c848ccc00401400800cd4d401088800c888008034030c11c0040ec0e84cc0a12401104552524f522d524553455256452d313000333573466e1d2000500203b03a103a50041039132350092233302200300200132323232333303f2375600207c66607e66074eb5d68219199a800919b804800000488cdc0001000919b8048000004cccc0fccc0e88dd580091bab00103e233350012333004330460450452122230030040012233333300975aeb41181180080048ccc010cc1181141148d54120004004cccccc0188dd580091bab00103e03e004001333303e2375600207a66002084466e092001001002303d75a606c01e606aa008646002002446607a0024466e00c028008ccc01001000c004888888c8cc084cc8c0040048cc1092f5bded8c0446464a66a6605400200e4266ae80cdd800119982580580519aa8269806801980600099803003002099aba0337600026660940140126aa09660180046600a00a0066aae7400cd55cf0010019800801191800800919820a5eb7bdb18088c8c94cd4cc0a400401c84cd5d019bb000233304a00a009335504c300c003300b00133006006004133574066ec0004ccc124024020c130c028008cc01401400cd55ce8019aab9e00213302123035491104552524f522d524553455256452d3037003020500113300c23034491104552524f522d524553455256452d30350033300d20013223002301300130224901104552524f522d524553455256452d31380053353300f22222222222232325335333573466e1d200235573a0040860842600201c2a0b06460020024466608ea0b244a66a6464a66a666ae68cdc7982080118208008240238999ab9a3370e6086004608600209008e208e6ae84d55cf0031aba135573c6ea80084488c0080104cc010010004004dd51aba1357440384a0964266605a6ae84d5d11aab9e37540024444666024400246660640024444660560120084646a002002c6602805a05846084930982024c266e95200033574066e9520023357406ea4ccc064049203c0134bd7025eb84103d87a800009980680e00d9191a8019119801801000980c9817800991919299a999ab9a3370e900000101a0198998219bae357420026eb8d5d09aba2001130324901035054310035573c0046aae74004dd519981f3a97526a6a60024440064440064002a660364605e9201104552524f522d524553455256452d3036000043016001330022302a4901104552524f522d524553455256452d303400333003200100733300420010190013300401a01922325335533533302f0012212230021233001005004002213500122122300212330010050041001213500122533533357346ae8c0040bc0b840084c0181004c00c0f5410088c88c008004c8c00400488ccc0bd2f5c044a66a600a00426466ae80c01c00c004cc0100100044cc01001000400488c88c008004c8c00400488ccc0b92f5c04466ae80c018c014008cc010010004004c05cd5d08009aab9e37540084666ae68cdc3a40046660166042002666014006901d0022441000250243232325335333573466e1d200000202602511222200415335333573466e1d200200202602511222200215335333573466e1d200400202602511222200115335333573466e1d2006002026025112222003130244901035054310035573c0046aae74004dd50019bae00414984c08124010350543500323001001222333023001223232323232332300100123302b00225335333573466ebc018d55ce80081481408020919aba00023300300300100330014bd6f7b6301aba03376000400666600e00e00600a6aae7400cd55cf0010011119299a998031ba933300400348100008004854cd4c8cc8c0040048cc09409c88cd40a0cc0ccdd71aab9d002375a6aae78008cc00c00c004004dd5800880f9109a80111299a8018999ab9a337120029000011812110812880f1998079aba135573c6ea800401c018888c94cd4cc8c004004894cd40044098884c8ccc06cd5d09aba235573c6ea800c8888c94cd4ccd5cd19b8748010d55ce8008148140a99a80110a99a991999999aba4001250412504123233330302504322253353504300321333303425047222533535046003213333038212230021233001009005222504d003046150490030421504500103e37580044a0824a0820786ae84d55cf001109a80091299a999ab9a3370e00402205a0582a66a666ae68cdc3a40046660260100249110e56657273696f6e206f7261636c650002d02c133503100400a100a100a1007100610063754004400466008008002664600200246604604a4466a04c004660060060026464664600200246604a0044466ae80008cc00c00c004008cc00804003ccc00488888888888802c984c0bc0c08854cd40044008884c0cc0d0c03cd5d09aab9e375400244466a660086ea400800c8c8cd4cc018dd480180091bad00148000dd5800a400044664600200246603ea0624a66a666ae68cdd78021aab9d00101d01c12303435573c00424660060060020024c444444444444010466601c002444464a66a666ae68cdc3a40086aae7400407006c4c8ccccccd5d2000928199281991919998111281a911299a9999999aba4003250382503823233330272503a222533533333303b225335333573466e1d20000020290281323253353300122122300212330010050040032135001223253353001003215335330062212230020040032153353003001211223002123300100600410081007100650401002302d001503e1503e2503d2503d2503d2503d00321333302b2503e2225335333333357480064a0824a08246466660604a086444a66a6a0840064266660684a08e444a66a6a08e00642666607042446004246600201200a444a09a00608c2a0920060842a08a00207c6eb001094104941040f084cccc0bc941088894cd4d410800c84cccc0cc8488c00848ccc00448c004038024014889412000c1045411000c0f45410000c0e4540f00040d4dd60021281c1281c0199099998131281c91119998149281e111299a9a81e0019099998169091180109199800806005802911282100181d8a81f00101b80181a0a81b8008181bac002250332503302e357426aae78004540c4dd50011281691299a800908008980101411191929991a998009aba100335742004264a66a60026ae84d5d100210a99a98011aba1357440084264a6466a666ae68cdc3a40006aae740080780744c94cd4ccd5cd19b8748000d55ce80080f80f0998039aba135573c0066ae84d55cf0008980124c6ea800c4c005262332330010052223330040072225335333573466e1c01800c09409054cd4ccd5cd19b870050020250241333573466e1c010004094090409040908088888c94cd4ccd5cd19b8748008d55ce800811010899191998029bad357420046eb4d5d08009bad357426ae88004d5d10009aab9e00113002498dd5001900f1baa002101a153353001357426ae8800c840684068c044800488c94c8cd4ccd5cd19b8748000d55ce80100e00d899299a999ab9a3370e90001aab9d00101d01c1333573466e3cdd71aba135573c0066eb8d5d09aab9e00101d01c13002498dd500189800a4c4a66a666ae68cdc3a40046aae7400807006c4c94cd4ccd5cd19b8748008d55ce80080e80e0999ab9a3371e6eb8d5d09aab9e003375c6ae84d55cf00080e80e080e1baa003101b3754004202e6aae78dd50011aab9e375400444a66a002202a266ae700080508d5d09aba235573c6ea800498888888888888030888c8c8c8c8c8c8c8c8c8c8c8cccccccccccc034dd61aba100b37586ae84028dd61aba100937566ae84020dd59aba100737586ae84018dd59aba1005357420086eb0d5d08019bab357420046eacd5d0800980b9aba1357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aab9e37540064c4444444444440144c4444008466600400244440044c444646464666600a6ae8400cdd59aba1002357420026600eeb8d5d09aba2001357440026ae88004d55cf1baa003223232325335333573466e1d200200200d00c1502215335333573466e1d200000200d00c13023300535742002260169201035054310035573c0046aae74004dd5000918011aba135573c6ea80048c8c8c94cd4ccd5cd19b87480000080280244dd71aba1001130084901035054310035573c0046aae74004dd500091bad357426ae88d55cf1baa001237566ae84d5d11aab9e3754002444646464a66a666ae68cdc3a400000401201026aa022600c6ae8400454cd4ccd5cd19b87480080080240204c04cc014d5d08008a99a999ab9a3370e9002001004804099aa80918031aba10013005357426ae880044c01d241035054310035573c0046aae74004dd5000919319ab9c0010141220021220012374c00244464600200246600a97adef6c602233574066ec0d55ce8011802980218031aab9e0023300300300122253335573e00220062660046ae84004d5d10008910010910911980080200191ba8001222333500123374a900119aba030030014bd701119ba548010cd5d0180280119aba030040014bd70119ba548000cd5d01802000a5eb8084888c00c01044884888cc0080140104484888c0040108c98cd5ce249024c6600007235001222374e66ae80d400c888dd399aba0350032333009752ea4004cd5d01a801111ba73357406ea4008cd5d01ba80014bd7019aba0375000297ae033574000466ae80dd4000a5eb80888d400488cdd2a400066ae80c014008cd5d01802000a5eb8088848ccc00401000c0088848cc00400c0088c98cd5ce249194552524f522d56455253494f4e2d43555252454e43592d303100002120011333330022250052500425004250042122300200313333300122500425003250032122300200325003222222533333357480022646600e6aae74004d55cf0009baa001130053756002260086eb00044c00cdd6800898011bae0012122300200311220011"

hexToShortBSUnsafe :: String -> BSS.ShortByteString
hexToShortBSUnsafe = BSS.toShort . (\case Right y -> y; Left _ -> error "xD") . B16.decode . C8.pack

aikenCompiledValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
aikenCompiledValidator = DeserializedCode (fmap (const mempty) prog) Nothing mempty
  where
    jazda = case deserialiseScript PlutusV2 (MajorProtocolVersion 9) (hexToShortBSUnsafe plutusValidator) of
      Right x -> x
      Left e -> error $ show e
    (ScriptNamedDeBruijn prog) = deserialisedScript jazda
