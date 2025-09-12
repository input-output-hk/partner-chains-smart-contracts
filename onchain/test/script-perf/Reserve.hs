module Reserve (
  execCosts,
) where

import Control.Lens
import PlutusLedgerApi.V1.Address qualified as Address
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptPerfUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.Reserve
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types qualified as Types

execCosts :: TestTree
execCosts =
  testGroup
    "Reserve scripts ExBudget"
    [ goldenPerf "reserveAuthPolicy" reserveAuthPolicy
    , goldenPerf "reserveValidatorDeposit" reserveValidatorDeposit
    , goldenPerf "reserveValidatorUpdate" reserveValidatorUpdate
    , goldenPerf "reserveValidatorTransferToICS" reserveValidatorTransferToICS
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
