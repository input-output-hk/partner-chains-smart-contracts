module Perf.Reserve (execCosts) where

import Prelude

import ApiBuilder
import Control.Lens
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as C8
import PartnerChains.ScriptId qualified as ScriptId
import PartnerChains.Scripts.Reserve
import PartnerChains.Types qualified as Types
import PlutusLedgerApi.V1.Address qualified as Address
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import PlutusTx.Code (CompiledCodeIn (..))
import Test.Tasty
import TestValues qualified as Test
import Testing

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
  aikenCode
    `appArg` vc
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx

aikenValidator :: String
aikenValidator = "010100229800aba2aba1aba0aab9faab9eaab9dab9a9bae0024888888896600264653001300900198049805000cdc3a400530090024888966002600460126ea800e33001300a3754007370e90024dc3a40013009375400891111919912cc004c0140122b3001301137540170028b20248acc004c0240122b3001301137540170028b20248acc004c0180122b3001301137540170028b20248acc004cdc3a400c00915980098089baa00b80145901245900f201e403c80786600244b300130053010375400513232332259800980c801c0162c80b0dd7180b0009bad301600230160013011375400516403d230133014301400192cc004c010c03cdd5000c528c528201c9ba54800244b300130093010375400514c103d87a8000898009bab301430113754004807a44646600200200644b30010018a5eb8226644b3001300500289980b80119802002000c4cc010010005013180b000980b800a02891809980a000a4444444664464b3001301d0018992cc004c078006264b3001301f0018cc0048966002602a60386ea800a20031375c6040603a6ea800901b48c07cc080c080c0800064603e60406040604060400032259800980a980e1baa0028800c4dd69810180e9baa002406d32330010013758603e60386ea804c896600200314c0103d87a80008992cc004cdd79810980f1baa0010108980619810000a5eb82266006006604400480e0c08000501e244445300133005300430203754601660406ea8c08c019ab30013014301f3754660126eacc00cc080dd500b99802980218101baa300b302037546046010d14a314a080f266e1d2001330023300d330093756600660406ea805ccc014c010c080dd5180598101baa3023007623300b0014881004800122259800980b98111baa004899192cc004c074c090dd5000c4c9660026054003132332259800980f18141baa00189991198008010cc004dd7181718159baa302e0029bae3016302b3754605c005301b302b375400a9112cc004c08cc0b4dd5000c4c8cc014004660026602266038660306eacc068c0bcdd500600211980d00080224001330113301c3301837566034605e6ea80240108cc06800401120009980d9bac301f302f375404c466ebcc0ccc0c0dd5181998181baa0013374a9001198191ba90104bd702444b300130360018cc004cc050cc07ccc06cdd5980e98191baa303500100723301d0010074800264660020026603e6eb0c0d8c0ccdd5015119baf303730343754606e60686ea8c07cc0d0dd500099ba548008cc0d8dd480a25eb808966002003148002266e00cc008008c0e0004cc058cc084cc074dd5980f981a1baa301f30343754606e00201246603e0020129000206a9980a1980f9980d9bab3015303237540526eb8c0d4dd6180e80491980e800a441004800260626ea80b12222598009815000c56600202b159800807c4cdc4803803452820688a5040d11598009817000c566002b30013375e6072010607201b13375e6042010604201b14a081a22b30013370e66e04dd6981300680119b81006007899b87337000086eb4c084dd6181080699b80337020046eb4c09803400e2941034452820688acc004c0ac0062b30010158acc0056600266ebcc0e4020c0e4036266e1cdd698130041bad302600d8a5040d113370e00c00f14a081a22941034456600202b15980080a44cdc399b800030070048a5040d114a081a1034206840d08b20661bac3031302e37540031640b03758605860526ea800488c8c8cc8966002606600713259800981218179baa0018991919912cc004c0e000e2646644b3001303b003806c590381bad3038001375c607000e607000d1640d46eb8c0d4004dd7181a801181a80098181baa0018b205c30320068b2060375a60600026eb0c0c0008c0c0004c0bc00a2c8138cdd7980b98139baa004301730273754002602e604e6ea8010c0a40062c8138cc88cc04c0088c966002604260506ea8006266e3c00cdd7181618149baa0018a50409c605660506ea8c0acc0a0dd50009bac3015302537540386eb8c0a0c094dd5000c59023181398121baa302730243754002601c60466ea8c098c08cdd500245902111640706600c6eb0c014c068dd500891919912cc004c04cc074dd5000c4c8cc04000456600200713370e6eb4c088c07cdd5000a407914a080e8c084c078dd5000c5901c180599804998029bab3007301c375400203246600e0029110e56657273696f6e204f7261636c6500300c301c3754002600c60366ea80062c80d8cc014dd61802180c9baa0102323322598009809180e1baa001899198078008acc00400e266e1cdd69810980f1baa001480ea294101c1810180e9baa0018b2036300a33008330043756600c60366ea80040608cc0180052210e56657273696f6e204f7261636c6500300b301b3754002600a60346ea80062c80d0cc010dd61801980c1baa00f2323322598009808980d9baa001899198070008acc00400e266e1cdd69810180e9baa00148102294101b180f980e1baa0018b2034300933007330033756600a60346ea800405c8cc0140052210e56657273696f6e204f7261636c6500300a301a3754002600860326ea800488c8cc00400400c896600200314c0103d87a8000899192cc004cdc8802800c56600266e3c014006260146603c603800497ae08a60103d87a80004069133004004302000340686eb8c068004c07400501b111919800800801912cc0040062980103d87a8000899192cc004cdc8802800c56600266e3c014006260146603c603800497ae08a60103d87a80004069133004004302000340686eb8c068004c07400501b0c048c04c014c044011164020300900130043754013149a26cac80101"

aikenCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
aikenCode = SerializedCode bs Nothing mempty
  where
    bs = case B16.decode (C8.pack aikenValidator) of
      Right x -> x
      _ -> error "Base16 decoding failed"
