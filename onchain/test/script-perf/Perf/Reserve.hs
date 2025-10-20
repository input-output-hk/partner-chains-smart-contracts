module Perf.Reserve (execCosts) where

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
import Prelude

import Data.ByteString.Short qualified as BSS

execCosts :: TestTree
execCosts =
  testGroup
    "Reserve scripts ExBudget"
    [ goldenPerf "reserveAuthPolicy" reserveAuthPolicy
    , goldenPerf "reserveValidatorDeposit" reserveValidatorDeposit
    , goldenPerf "aikenReserveValidatorDeposit" aikenReserveValidatorDeposit
    , goldenPerf "reserveValidatorUpdate" reserveValidatorUpdate
    , goldenPerf "aikenReserveValidatorUpdate" aikenReserveValidatorUpdate
    , goldenPerf "reserveValidatorTransferToICS" reserveValidatorTransferToICS
    , goldenPerf "aikenReserveValidatorTransferToICS" aikenReserveValidatorTransferToICS
    , goldenPerf "reserveValidatorHandover" reserveValidatorHandover
    , goldenPerf "aikenReserveValidatorHandover" aikenReserveValidatorHandover
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

aikenReserveValidatorDeposit :: CompiledCode BuiltinUnit
aikenReserveValidatorDeposit =
  runAikenValidator
    Test.versionOracleConfig
    (wrapToVersionedData reserveDatum)
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

aikenReserveValidatorUpdate :: CompiledCode BuiltinUnit
aikenReserveValidatorUpdate =
  runAikenValidator
    Test.versionOracleConfig
    (wrapToVersionedData reserveDatum)
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
    (wrapToVersionedData reserveDatum)
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

aikenReserveValidatorHandover :: CompiledCode BuiltinUnit
aikenReserveValidatorHandover =
  runAikenValidator
    Test.versionOracleConfig
    (wrapToVersionedData reserveDatum)
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

wrapToVersionedData :: (ToData a) => a -> BuiltinData
wrapToVersionedData datum =
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
  appArgsUPLC
    aikenCompiledValidator
    [ toBuiltinData vc
    , toBuiltinData datum
    , toBuiltinData redeemer
    , toBuiltinData ctx
    ]

-- runAikenValidator :: Types.VersionOracleConfig -> BuiltinData -> Types.ReserveRedeemer -> V2.ScriptContext -> CompiledCode BuiltinUnit
-- runAikenValidator vc datum redeemer ctx =
--   aikenCompiledValidator
--     `appArg110` vc
--     `appArg110` datum
--     `appArg110` redeemer
--     `appArg110` ctx

aikenValidator :: String
aikenValidator = "591df3010100229800aba4aba2aba1aba0aab9faab9eaab9dab9cab9a9bae0024888888888a60022a6600692013165787065637420726573657276655f6f75747075745f646174756d3a2057696b7361203d206f75747075745f646174756d00168a99801a493465787065637420496e6c696e65446174756d286f75747075745f646174756d29203d206f75747075745f7574786f2e646174756d00168a99801a4926657870656374205b6f75747075745f7574786f5d203d20726573657276655f6f75747075747300168a99801a49ff657870656374205b676f7665726e616e63655265666572656e6365496e7075745d203d0a2020202020202020202073656c662e7265666572656e63655f696e707574730a2020202020202020202020207c3e206c6973742e66696c746572280a20202020202020202020202020202020666e28696e70757429207b0a2020202020202020202020202020202020206c6574206f7574707574203d20696e7075742e6f75747075740a2020202020202020202020202020202020206c6574206861735f746f6b656e203d0a20202020202020202020202020202020202020206173736574732e746f5f64696374286f75747075742e76616c7565290a20202020ff2020202020202020202020202020202020207c3e20646963742e6765742876657273696f6e5f6f7261636c655f636f6e666967290a202020202020202020202020202020202020202020207c3e206f7074696f6e2e616e645f7468656e28646963742e676574285f2c202256657273696f6e206f7261636c652229290a202020202020202020202020202020202020202020207c3e206f7074696f6e2e69735f736f6d6528290a0a2020202020202020202020202020202020207768656e206f75747075742e646174756d206973207b0a2020202020202020202020202020202020202020496e6c696e65446174756d286461746129202d3e207b0a202020ff2020202020202020202020202020202020202020206966206461746120697320646174756d3a2056657273696f6e696e67446174756d207b0a2020202020202020202020202020202020202020202020206861735f746f6b656e20262620646174756d2e7363726970744964203d3d2033320a202020202020202020202020202020202020202020207d20656c7365207b0a20202020202020202020202020202020202020202020202046616c73650a202020202020202020202020202020202020202020207d0a202020202020202020202020202020202020202020207d0a20202020202020202020202020202020202020205f202d3e2046616c73650a362020202020202020202020202020202020207d0a202020202020202020202020202020207d2c0a20202020202020202020202020202900168a99801a49ff657870656374205b6f75747075745f6963735f7574786f5d203d0a20202020202020202020202020202020202073656c662e6f7574707574730a20202020202020202020202020202020202020207c3e206c6973742e66696c746572280a202020202020202020202020202020202020202020202020666e286f757470757429207b0a20202020202020202020202020202020202020202020202020206c65742061646472203d206f75747075742e616464726573730a2020202020202020202020202020202020202020202020202020616464722e7061796d656e745f63726564656e7469616c203d3d20536372697074280a2020202020202020202020882020202020202020202020202020202020696c6c69717569645f63697263756c6174696f6e5f737570706c795f7363726970745f686173682c0a2020202020202020202020202020202020202020202020202020290a2020202020202020202020202020202020202020202020207d2c0a202020202020202020202020202020202020202020202900168a99801a49ff657870656374205b696c6c697175696443697263756c6174696f6e537570706c795265666572656e6365496e7075745d203d0a2020202020202020202073656c662e7265666572656e63655f696e707574730a2020202020202020202020207c3e206c6973742e66696c746572280a20202020202020202020202020202020666e28696e70757429207b0a2020202020202020202020202020202020206c6574206f7574707574203d20696e7075742e6f75747075740a2020202020202020202020202020202020206c6574206861735f746f6b656e203d0a20202020202020202020202020202020202020206173736574732e746f5f64696374286f7574ff7075742e76616c7565290a202020202020202020202020202020202020202020207c3e20646963742e6765742876657273696f6e5f6f7261636c655f636f6e666967290a202020202020202020202020202020202020202020207c3e206f7074696f6e2e616e645f7468656e28646963742e676574285f2c202256657273696f6e206f7261636c652229290a202020202020202020202020202020202020202020207c3e206f7074696f6e2e69735f736f6d6528290a0a2020202020202020202020202020202020207768656e206f75747075742e646174756d206973207b0a2020202020202020202020202020202020202020496e6c696e65446174756dff286461746129202d3e207b0a2020202020202020202020202020202020202020202020206966206461746120697320646174756d3a2056657273696f6e696e67446174756d207b0a2020202020202020202020202020202020202020202020206861735f746f6b656e20262620646174756d2e7363726970744964203d3d2033300a202020202020202020202020202020202020202020207d20656c7365207b0a20202020202020202020202020202020202020202020202046616c73650a202020202020202020202020202020202020202020207d0a202020202020202020202020202020202020202020207d0a2020202020202020202020202020202045202020205f202d3e2046616c73650a2020202020202020202020202020202020207d0a202020202020202020202020202020207d2c0a20202020202020202020202020202900168a99801a492f65787065637420726573657276655f696e7075745f646174756d3a2057696b7361203d20696e7075745f646174756d00168a99801a49ff657870656374205b7265736572766541757468546f6b656e5265666572656e6365496e7075745d203d0a20202020202073656c662e7265666572656e63655f696e707574730a20202020202020207c3e206c6973742e66696c746572280a202020202020202020202020666e28696e70757429207b0a20202020202020202020202020206c6574206f7574707574203d20696e7075742e6f75747075740a20202020202020202020202020206c6574206861735f746f6b656e203d0a202020202020202020202020202020206173736574732e746f5f64696374286f75747075742e76616c7565290a2020202020202020202020202020202020207c3e2064ff6963742e6765742876657273696f6e5f6f7261636c655f636f6e666967290a2020202020202020202020202020202020207c3e206f7074696f6e2e616e645f7468656e28646963742e676574285f2c202256657273696f6e206f7261636c652229290a2020202020202020202020202020202020207c3e206f7074696f6e2e69735f736f6d6528290a0a20202020202020202020202020207768656e206f75747075742e646174756d206973207b0a20202020202020202020202020202020496e6c696e65446174756d286461746129202d3e207b0a20202020202020202020202020202020202020206966206461746120697320646174756d3a20566572e773696f6e696e67446174756d207b0a20202020202020202020202020202020202020206861735f746f6b656e20262620646174756d2e7363726970744964203d3d2032390a2020202020202020202020202020202020207d20656c7365207b0a202020202020202020202020202020202020202046616c73650a2020202020202020202020202020202020207d0a2020202020202020202020202020202020207d0a202020202020202020202020202020205f202d3e2046616c73650a20202020202020202020202020207d0a2020202020202020202020207d2c0a202020202020202020202900168a99801a493365787065637420726573657276655f72656465656d65723a205265736572766552656465656d6572203d2072656465656d657200164888888889660026465300130130019809980a000cdc3a400530130024888966002600460266ea800e2646644a6602a9210648454c4c4f3500159800803c4cc88cc89660026006003159800980d9baa00b801403901c45660026012003159800980d9baa00b801403901c45660026008003159800980d9baa00b801403901c456600266e1d20060018acc004c06cdd5005c00a01c80e201c80c1018203040603300122259800801c006264b3001001801400a0051332259800800c012264b3001001802c01600b0058992cc004c09400e00f00640886eb80050251811000a040375a00260420090024088603e00680ea4603a603c603c0032598009801180c9baa0018a518a50405d374a900048966002601260346ea800a298103d87a8000898009bab301e301b375400480c244646600200200644b30010018a5eb8226644b3001300500289981080119802002000c4cc01001000501c18100009810800a03c9180e980f000a4444444664464b300100180bc4c966002605000519800912cc004c050c094dd50014400626eb8c0a4c098dd5001204691814181498149814800c89660026028604a6ea800a20031375a6052604c6ea800902348c0a0c0a4c0a4c0a4c0a400664660020026eb0c0a0c094dd5009112cc0040062980103d87a80008992cc004cdd7981518139baa0010128980599814800a5eb8226600600660560048120c0a400502724444464b30013012302937540051323259800980d18159baa001899912cc004c05cc0b4dd5000c4c96600200302481244cc8a6002005198009bae30343031375460686eb0c0d000a6eb8c048c0c4dd5181a1bac3034002998059980a198081bab300a3031375403c6eb8c0d0dd618091bac3034002233012001488100480026eb4c05cdd6181a0014966002603460626ea80062946294102f4888c8cc00400401089660020031004899801981c80099801001181d000a06e98181baa0219b87481012222222232598009811001c56600201d0338992cc004c0fc03e330013375e604260766ea8044c084c0ecdd5000ccc054cc078cc068dd5980e181d9baa00100a23301c00100a480026603a6eb0c070c0ecdd501411919912cc004c0a0c0f8dd5000c4cc8966002005001800c4ca6002003159800802c4c028dd69822800c528207e80120523758005001800a08c3042303f37540022942294103c1811198101980e1bab301e303d375400207246603c00291010e56657273696f6e206f7261636c65003023303d3754002603a60786ea800522259800800c0da264b300130430028acc0056600260146603c6eacc060c0fcdd50161980d980d181f9baa3020303f3754002a6607a9210e6d697373696e672073637269707400168a518a9981ea49176d696e7473476f7665726e616e6365203f2046616c73650014a081e22b3001598008024528c54cc0f524011d646174756d5f646f65735f6e6f745f6368616e6765203f2046616c73650014a081e22b30013371200c00714a3153303d4901356f75747075745f726573657276655f746f6b656e73203e3d20696e7075745f726573657276655f746f6b656e73203f2046616c73650014a081e2294103c4528207881ba080304100140fc81a2078303d00e40ed132598009814802456600201f0348992cc004c100042264b30013026303c375400313259800800c0e60731329800800c6600266e1ccdc080580619b8133019330223301e37566040607e6ea80100388cc0800040392000006acc004cdd798211bac304200130423758608402113375e60406eb0c108004c080dd6182100845282078998109bac3020303f37540584646644b3001302c30423754003133225980080140060031329800800c56600200b13370e6eb4c124005203c8a50410d00240b46eb000a0030014128608c60866ea800452845282080302633024330203756604460826ea80040f48cc08800522010e56657273696f6e206f7261636c6500302730413754002604260806ea800522259800800c0e2264b30013047002899192cc0040060791325980098250014566002b30010068a518a99822248122646174756d5f6368616e67655f6f6e6c795f62795f7374617473203f2046616c73650014a0821a2b300159800803c528c54cc11124012661737365745f6368616e67655f62795f636f72726563745f616d6f756e74203f2046616c73650014a0821a2b30013370e66e00cc080cc0a4cc094dd5981398231baa00101523302700101548000dd698139bac30273758609202e66e00cdc08098094c004cc0a0dd6182498231baa03323375e6094608e6ea8c128c11cdd5181418239baa001300d33049375200897ae0a4001223370000266044660566604e6eacc0a4c120dd5181498241baa002017233029001017480010104528c54cc111240129636f72726563745f616d6f756e745f7472616e736665727265645f746f5f696373203f2046616c73650014a0821a29410434528208681ea08e304800141186604c6eb0c0a8c110dd5018919baf3048304537546090608a6ea8004c02ccc11cdd480125eb80cc07cc078c10cdd5181218219baa0015330414910e6d697373696e6720736372697074001681ca0883045001410c81d201e375800303981ca0863040303d375400303740e8604460786ea800606a81e8c0f803d03c45660026048009159800807c0d2264b300130400108992cc004c098c0f0dd5000c4c96600200303981cc4ca6002003198009980c998111980f1bab3020303f375400801c46604000201c900056600266ebcc108dd6182100098211bac3042010899b87375a604a6eb0c108004dd698129bac30420108a5040f13302137586040607e6ea80b08c8cc8966002605860846ea800626644b3001002800c00626530010018acc0040162601c6eb4c1240062941043400902d1bac002800c00504a182318219baa00114a114a08200c098cc090cc080dd5981118209baa00103d23302200148810e56657273696f6e206f7261636c6500302730413754002604260806ea800522259800800c0ea264b300130470028acc00566002601c660446eacc070c10cdd50181980f980f18219baa302430433754002a660829210e6d697373696e672073637269707400168a518a99820a49176d696e7473476f7665726e616e6365203f2046616c73650014a082022b300159800801c528c54cc10524012d646174756d5f6368616e67655f6f6e6c795f62795f6d757461626c655f73657474696e6773203f2046616c73650014a082022b30013370e00801514a315330414901356f75747075745f726573657276655f746f6b656e73203d3d20696e7075745f726573657276655f746f6b656e73203f2046616c73650014a0820229410404528208081da0883045001410c81d201e375800303981ca0863040303d375400303740e8604460786ea800606a81e8c0f803d03c456600201f13259800800c0ca264b30013041002899192cc00400606d13259800982200144cc896600200303a8992cc004c11c00a2b3001598009807198111bab301c304337540606603e603c60866ea8c090c10cdd5000a99820a4810e6d697373696e672073637269707400168a518a99820a49176d696e7473476f7665726e616e6365203f2046616c73650014a082022b30015980080d4528c54cc10524011a6275726e735f726573657276655f61757468203f2046616c73650014a082022b30010038a518a99820a4812c616c6c5f726573657276655f746f6b656e735f7472616e7366657265645f746f5f696373203f2046616c73650014a0820229410404528208081da0883045001410c66e1ccdc04c004cc088dd6182198201baa02d23375e608860826ea8c110c104dd5181118209baa001300733043375200897ae0a40012233700002660386604a660426eacc08cc108dd5181198211baa0020112330230010114800100a0039980d198119980f9bab30213040375400201e46604200201e9000198111bac30213040375405a4646644b3001302d30433754003133225980080140060031329800800c56600200b1300f375a609400314a082220048170dd600140060028258c11cc110dd50008a508a504104604e6604a660426eacc08cc108dd500081f119811800a450e56657273696f6e206f7261636c6500302830423754002604460826ea800606e8208c108005040198101bac3024303e3754056466ebcc108c0fcdd51821181f9baa001300533041375200497ae0330193018303d3754603c607a6ea80054cc0ed24010e6d697373696e67207363726970740016819a07c303f00140f46603a6eb0c070c0ecdd501411919912cc004c0a0c0f8dd5000c4cc8966002005001800c4ca6002003159800802c4cdc39bad3045001480f2294103f40090291bac002800c0050461821181f9baa00114a114a081e0c088cc080cc070dd5980f181e9baa00103923301e00148810e56657273696f6e206f7261636c65003023303d3754002603a60786ea80062a660729211b657870656374205b5d203d20726573657276655f6f757470757473001640f081c10381ba548009037198099980e1980c1bab301a3039375401e0104660340020109000204c8008dd60009112cc00400e00313259800800c00a0051332259800800c01226644b300100180344c966002003007803c01e264b3001303d0038acc00401a01113259800800c4c96600200300a8992cc00400601700b899912cc00400601b13259800800c03a01d00e8992cc004c11000e2b30013028303f375400f13259800800c042264b3001001808c046023011899912cc00400602713259800800c05202901480a44c96600260940071598008054056264b300100180b405a02d016899912cc00400603113259800800c0660330198992cc004c13c00e03701a41306eb40060328278c13000504a1bae001304b00b41306092014823a02a8238dd7000a094304700141146eb8004c1180090471822000a0843040375400f00f40f500f41046eb400601c8220c10400503f1bac0013040002805c02d041181f000a078303e007804c02601300940fc607800c81d201081d0dd6800c01d03d181d000a0703038001303900140d86eb0004c0dc01200500240e0606a006819a04902440d06062605c6ea80062a6605892013265787065637420496e6c696e65446174756d28696e7075745f646174756d29203d20696e7075745f7574786f2e646174756d001640ac664466020004464b3001301e302f375400313371e0066eb8c0ccc0c0dd5000c528205a3032302f37546064605e6ea8004dd6180918161baa019375c605e60586ea8004c048c0b0dd5001454cc0a924014a6578706563742053637269707428726573657276655f7363726970745f6861736829203d20696e7075745f7574786f2e616464726573732e7061796d656e745f63726564656e7469616c001640a4605c60566ea8c0b8c0acdd5000980598151baa302d302a3754005153302849014e65787065637420536f6d6528696e70757429203d0a20202020202073656c662e696e707574730a20202020202020207c3e207472616e73616374696f6e2e66696e645f696e707574287574786f290016409c66e1d2001330033300c330083756600460526ea8058cc014c010c0a4dd5180518149baa00653302749010e6d697373696e6720736372697074001623300a00148900480010184094604c0028120cc010dd6180198111baa00f232332259800980798129baa001899912cc00400a0030018994c0040062b3001005899b87375a6058002901d4528204c80120203758005001800a05a30293026375400229422941023180499803998019bab30053024375400204046600a0029110e56657273696f6e206f7261636c6500300a30243754002600860466ea800488c8cc00400400c896600200314c0103d87a8000899192cc004cdc8802800c56600266e3c0140062601466050604c00497ae08a60103d87a8000408d133004004302a003408c6eb8c090004c09c005025111919800800801912cc0040062980103d87a8000899192cc004cdc8802800c56600266e3c0140062601466050604c00497ae08a60103d87a8000408d133004004302a003408c6eb8c090004c09c0050250c060dd50049b8748010dc3a400100a805402a01480e0c060004c060c064004c050dd5001c590110c04c004c038dd500a45268a998062491856616c696461746f722072657475726e65642066616c73650013656402c1"

-- plutusValidator :: String
-- plutusValidator = "5911b70100003322323232323232323232323332223232332232323233223232323232323232323232323232323232323232323232323232323222225335323232323232323232323235330182302c4901104552524f522d524553455256452d3036000012223232323232323232325323323233350191533533029491104552524f522d524553455256452d3031003302001a01b1533533029491104552524f522d524553455256452d3032003232532335333573466e1d200035573a6ea800c0fc0f854cd4ccd5cd19b8748000d55ce9baa00203f03e103f130014984c00526232532335333573466e1d200235573a004082080264a66a666ae68cdc3a40046aae740041081044ccd5cd19b8f375c6ae84d55cf0019bae357426aae780041081044c00926375400826002931299a999ab9a3370e90021aab9d0020410401325335333573466e1d200435573a0020840822666ae68cdd79aba135573c0066ae84d55cf00082102088209baa004104037540066066a0106064022266052921104552524f522d524553455256452d303300333573466e25400d200003b03c103b103b15335330294901104552524f522d524553455256452d3135003302001a01b1533533029491104552524f522d524553455256452d313600333573466e1d2001333022500933302101a480e806d2210003c03b1330294901104552524f522d524553455256452d313700333573466e1d4008cdc01805008a80081e01d881d881d89919299a99815a49104552524f522d524553455256452d313100333573466e1d4014cdc080080101f01e8a99a99815a481104552524f522d524553455256452d313200333573466ebcc128ccc130d4c03888800c88848ccc00401000c01804003cc12940200f80f44cc0ad2401104552524f522d524553455256452d313300333573466e1ccdc028021a9a98071110019110011100099b8033702004002a00607c07a207a207a6a601a444006444002666044a0126eb8dd49a9a980611100191100111001245001323300448000004c8ccc05480040a8ccc05080048cc0a4ccc0d0d5d09aba235573c6ea80040c80c4008049401c4c020cc0448c0e52401104552524f522d524553455256452d3134003233301320012330283330330010310300023301502e02d50061533533027491104552524f522d524553455256452d3038003301e018019132533533028491104552524f522d524553455256452d303900333573466ebcc11cccc124d4c02c88800c888c848ccc00401400800cd4d401088800c888008034030c11c0040ec0e84cc0a12401104552524f522d524553455256452d313000333573466e1d2000500203b03a103a50041039132350092233302200300200132323232333303f2375600207c66607e66074eb5d68219199a800919b804800000488cdc0001000919b8048000004cccc0fccc0e88dd580091bab00103e233350012333004330460450452122230030040012233333300975aeb41181180080048ccc010cc1181141148d54120004004cccccc0188dd580091bab00103e03e004001333303e2375600207a66002084466e092001001002303d75a606c01e606aa008646002002446607a0024466e00c028008ccc01001000c004888888c8cc084cc8c0040048cc1092f5bded8c0446464a66a6605400200e4266ae80cdd800119982580580519aa8269806801980600099803003002099aba0337600026660940140126aa09660180046600a00a0066aae7400cd55cf0010019800801191800800919820a5eb7bdb18088c8c94cd4cc0a400401c84cd5d019bb000233304a00a009335504c300c003300b00133006006004133574066ec0004ccc124024020c130c028008cc01401400cd55ce8019aab9e00213302123035491104552524f522d524553455256452d3037003020500113300c23034491104552524f522d524553455256452d30350033300d20013223002301300130224901104552524f522d524553455256452d31380053353300f22222222222232325335333573466e1d200235573a0040860842600201c2a0b06460020024466608ea0b244a66a6464a66a666ae68cdc7982080118208008240238999ab9a3370e6086004608600209008e208e6ae84d55cf0031aba135573c6ea80084488c0080104cc010010004004dd51aba1357440384a0964266605a6ae84d5d11aab9e37540024444666024400246660640024444660560120084646a002002c6602805a05846084930982024c266e95200033574066e9520023357406ea4ccc064049203c0134bd7025eb84103d87a800009980680e00d9191a8019119801801000980c9817800991919299a999ab9a3370e900000101a0198998219bae357420026eb8d5d09aba2001130324901035054310035573c0046aae74004dd519981f3a97526a6a60024440064440064002a660364605e9201104552524f522d524553455256452d3036000043016001330022302a4901104552524f522d524553455256452d303400333003200100733300420010190013300401a01922325335533533302f0012212230021233001005004002213500122122300212330010050041001213500122533533357346ae8c0040bc0b840084c0181004c00c0f5410088c88c008004c8c00400488ccc0bd2f5c044a66a600a00426466ae80c01c00c004cc0100100044cc01001000400488c88c008004c8c00400488ccc0b92f5c04466ae80c018c014008cc010010004004c05cd5d08009aab9e37540084666ae68cdc3a40046660166042002666014006901d0022441000250243232325335333573466e1d200000202602511222200415335333573466e1d200200202602511222200215335333573466e1d200400202602511222200115335333573466e1d2006002026025112222003130244901035054310035573c0046aae74004dd50019bae00414984c08124010350543500323001001222333023001223232323232332300100123302b00225335333573466ebc018d55ce80081481408020919aba00023300300300100330014bd6f7b6301aba03376000400666600e00e00600a6aae7400cd55cf0010011119299a998031ba933300400348100008004854cd4c8cc8c0040048cc09409c88cd40a0cc0ccdd71aab9d002375a6aae78008cc00c00c004004dd5800880f9109a80111299a8018999ab9a337120029000011812110812880f1998079aba135573c6ea800401c018888c94cd4cc8c004004894cd40044098884c8ccc06cd5d09aba235573c6ea800c8888c94cd4ccd5cd19b8748010d55ce8008148140a99a80110a99a991999999aba4001250412504123233330302504322253353504300321333303425047222533535046003213333038212230021233001009005222504d003046150490030421504500103e37580044a0824a0820786ae84d55cf001109a80091299a999ab9a3370e00402205a0582a66a666ae68cdc3a40046660260100249110e56657273696f6e206f7261636c650002d02c133503100400a100a100a1007100610063754004400466008008002664600200246604604a4466a04c004660060060026464664600200246604a0044466ae80008cc00c00c004008cc00804003ccc00488888888888802c984c0bc0c08854cd40044008884c0cc0d0c03cd5d09aab9e375400244466a660086ea400800c8c8cd4cc018dd480180091bad00148000dd5800a400044664600200246603ea0624a66a666ae68cdd78021aab9d00101d01c12303435573c00424660060060020024c444444444444010466601c002444464a66a666ae68cdc3a40086aae7400407006c4c8ccccccd5d2000928199281991919998111281a911299a9999999aba4003250382503823233330272503a222533533333303b225335333573466e1d20000020290281323253353300122122300212330010050040032135001223253353001003215335330062212230020040032153353003001211223002123300100600410081007100650401002302d001503e1503e2503d2503d2503d2503d00321333302b2503e2225335333333357480064a0824a08246466660604a086444a66a6a0840064266660684a08e444a66a6a08e00642666607042446004246600201200a444a09a00608c2a0920060842a08a00207c6eb001094104941040f084cccc0bc941088894cd4d410800c84cccc0cc8488c00848ccc00448c004038024014889412000c1045411000c0f45410000c0e4540f00040d4dd60021281c1281c0199099998131281c91119998149281e111299a9a81e0019099998169091180109199800806005802911282100181d8a81f00101b80181a0a81b8008181bac002250332503302e357426aae78004540c4dd50011281691299a800908008980101411191929991a998009aba100335742004264a66a60026ae84d5d100210a99a98011aba1357440084264a6466a666ae68cdc3a40006aae740080780744c94cd4ccd5cd19b8748000d55ce80080f80f0998039aba135573c0066ae84d55cf0008980124c6ea800c4c005262332330010052223330040072225335333573466e1c01800c09409054cd4ccd5cd19b870050020250241333573466e1c010004094090409040908088888c94cd4ccd5cd19b8748008d55ce800811010899191998029bad357420046eb4d5d08009bad357426ae88004d5d10009aab9e00113002498dd5001900f1baa002101a153353001357426ae8800c840684068c044800488c94c8cd4ccd5cd19b8748000d55ce80100e00d899299a999ab9a3370e90001aab9d00101d01c1333573466e3cdd71aba135573c0066eb8d5d09aab9e00101d01c13002498dd500189800a4c4a66a666ae68cdc3a40046aae7400807006c4c94cd4ccd5cd19b8748008d55ce80080e80e0999ab9a3371e6eb8d5d09aab9e003375c6ae84d55cf00080e80e080e1baa003101b3754004202e6aae78dd50011aab9e375400444a66a002202a266ae700080508d5d09aba235573c6ea800498888888888888030888c8c8c8c8c8c8c8c8c8c8c8cccccccccccc034dd61aba100b37586ae84028dd61aba100937566ae84020dd59aba100737586ae84018dd59aba1005357420086eb0d5d08019bab357420046eacd5d0800980b9aba1357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aab9e37540064c4444444444440144c4444008466600400244440044c444646464666600a6ae8400cdd59aba1002357420026600eeb8d5d09aba2001357440026ae88004d55cf1baa003223232325335333573466e1d200200200d00c1502215335333573466e1d200000200d00c13023300535742002260169201035054310035573c0046aae74004dd5000918011aba135573c6ea80048c8c8c94cd4ccd5cd19b87480000080280244dd71aba1001130084901035054310035573c0046aae74004dd500091bad357426ae88d55cf1baa001237566ae84d5d11aab9e3754002444646464a66a666ae68cdc3a400000401201026aa022600c6ae8400454cd4ccd5cd19b87480080080240204c04cc014d5d08008a99a999ab9a3370e9002001004804099aa80918031aba10013005357426ae880044c01d241035054310035573c0046aae74004dd5000919319ab9c0010141220021220012374c00244464600200246600a97adef6c602233574066ec0d55ce8011802980218031aab9e0023300300300122253335573e00220062660046ae84004d5d10008910010910911980080200191ba8001222333500123374a900119aba030030014bd701119ba548010cd5d0180280119aba030040014bd70119ba548000cd5d01802000a5eb8084888c00c01044884888cc0080140104484888c0040108c98cd5ce249024c6600007235001222374e66ae80d400c888dd399aba0350032333009752ea4004cd5d01a801111ba73357406ea4008cd5d01ba80014bd7019aba0375000297ae033574000466ae80dd4000a5eb80888d400488cdd2a400066ae80c014008cd5d01802000a5eb8088848ccc00401000c0088848cc00400c0088c98cd5ce249194552524f522d56455253494f4e2d43555252454e43592d303100002120011333330022250052500425004250042122300200313333300122500425003250032122300200325003222222533333357480022646600e6aae74004d55cf0009baa001130053756002260086eb00044c00cdd6800898011bae0012122300200311220011"

hexToShortBSUnsafe :: String -> BSS.ShortByteString
hexToShortBSUnsafe = BSS.toShort . (\case Right y -> y; Left e -> error (show e)) . B16.decode . C8.pack

aikenCompiledValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
aikenCompiledValidator = DeserializedCode (fmap (const mempty) prog) Nothing mempty
  where
    ds = case deserialiseScript PlutusV3 (MajorProtocolVersion 9) (hexToShortBSUnsafe aikenValidator) of
      Right x -> x
      Left e -> error $ show e
    (ScriptNamedDeBruijn prog) = deserialisedScript ds
