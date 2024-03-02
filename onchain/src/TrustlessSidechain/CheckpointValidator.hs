{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TrustlessSidechain.CheckpointValidator (
  mkCheckpointValidator,
  serializeCheckpointMsg,
  initCheckpointMintTn,
  initCheckpointMintAmount,
  mkCheckpointPolicy,
  mkCheckpointPolicyUntyped,
  serialisableCheckpointPolicy,
  mkCheckpointValidatorUntyped,
  serialisableCheckpointValidator,
) where

import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  OutputDatum (OutputDatum),
  Script,
  ScriptContext (scriptContextTxInfo),
  TokenName (TokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
  Value (getValue),
  fromCompiledCode,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ATMSPlainAggregatePubKey,
  CheckpointDatum,
  CheckpointMessage (
    CheckpointMessage,
    blockHash,
    blockNumber,
    sidechainEpoch,
    sidechainParams
  ),
  CheckpointParameter,
  InitTokenAssetClass,
  SidechainParams,
  UpdateCommitteeDatum,
 )
import TrustlessSidechain.Utils (
  CurrencySymbolRaw (CurrencySymbolRaw),
  ScriptContextRaw (ScriptContextRaw),
  oneTokenBurned,
  safeGetMinting,
  safeGetScriptPurpose,
  safeGetTxInfo,
  safeGetTxInfoMint,
  unTxInfoMintRaw,
 )
import TrustlessSidechain.Versioning (
  VersionOracle (VersionOracle),
  VersionOracleConfig,
  committeeCertificateVerificationPolicyId,
  committeeOraclePolicyId,
  getVersionedCurrencySymbol,
  scriptId,
  version,
 )

serializeCheckpointMsg :: CheckpointMessage -> BuiltinByteString
serializeCheckpointMsg = Builtins.serialiseData . IsData.toBuiltinData

{-# INLINEABLE mkCheckpointValidator #-}
-- OnChain error descriptions:
--
--   ERROR-CHECKPOINT-VALIDATOR-01: Output missing NFT.
--
--   ERROR-CHECKPOINT-VALIDATOR-02: Committee signature invalid.
--
--   ERROR-CHECKPOINT-VALIDATOR-03: New checkpoint block number must be greater
--   than current checkpoint block number.
--
--   ERROR-CHECKPOINT-VALIDATOR-04: New checkpoint block hash must be different
--   from current checkpoint block hash.
--
--   ERROR-CHECKPOINT-VALIDATOR-05: No committee UTxO given as reference input.
--
--   ERROR-CHECKPOINT-VALIDATOR-06: Expected exactly one continuing output.
--
--   ERROR-CHECKPOINT-VALIDATOR-07: Missing output inline datum.
mkCheckpointValidator ::
  CheckpointParameter ->
  VersionOracleConfig ->
  CheckpointDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkCheckpointValidator checkpointParam versioningConfig datum _red ctx =
  traceIfFalse "ERROR-CHECKPOINT-VALIDATOR-01" outputContainsCheckpointNft
    && traceIfFalse "ERROR-CHECKPOINT-VALIDATOR-02" signedByCurrentCommittee
    && traceIfFalse
      "ERROR-CHECKPOINT-VALIDATOR-03"
      (get @"blockNumber" datum < get @"blockNumber" outputDatum)
    && traceIfFalse
      "ERROR-CHECKPOINT-VALIDATOR-04"
      (get @"blockHash" datum /= get @"blockHash" outputDatum)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    committeeOracleCurrencySymbol :: CurrencySymbol
    committeeOracleCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = committeeOraclePolicyId})
        ctx

    committeeCertificateVerificationCurrencySymbol :: CurrencySymbol
    committeeCertificateVerificationCurrencySymbol =
      getVersionedCurrencySymbol
        versioningConfig
        (VersionOracle {version = 1, scriptId = committeeCertificateVerificationPolicyId})
        ctx

    minted :: Value
    minted = txInfoMint info

    sc :: SidechainParams
    sc = get @"sidechainParams" checkpointParam

    -- Check if the transaction input value contains the current committee NFT
    containsCommitteeNft :: TxInInfo -> Bool
    containsCommitteeNft txIn =
      let resolvedOutput = txInInfoResolved txIn
          outputValue = txOutValue resolvedOutput
          amount = Value.valueOf outputValue committeeOracleCurrencySymbol (TokenName "")
       in amount == 1

    -- Extract the UpdateCommitteeDatum from the list of input transactions
    extractCommitteeDatum :: [TxInInfo] -> UpdateCommitteeDatum ATMSPlainAggregatePubKey
    extractCommitteeDatum [] = traceError "ERROR-CHECKPOINT-VALIDATOR-05"
    extractCommitteeDatum (txIn : txIns)
      | containsCommitteeNft txIn = case txOutDatum (txInInfoResolved txIn) of
        OutputDatum d -> IsData.unsafeFromBuiltinData $ getDatum d
        _ -> extractCommitteeDatum txIns
      | otherwise = extractCommitteeDatum txIns

    committeeDatum :: UpdateCommitteeDatum ATMSPlainAggregatePubKey
    committeeDatum = extractCommitteeDatum (txInfoReferenceInputs info)

    ownOutput :: TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "ERROR-CHECKPOINT-VALIDATOR-06"

    outputDatum :: CheckpointDatum
    outputDatum = case txOutDatum ownOutput of
      OutputDatum d -> IsData.unsafeFromBuiltinData (getDatum d)
      _ -> traceError "ERROR-CHECKPOINT-VALIDATOR-07"

    -- TODO: query currency symbol from versioning system (https://github.com/input-output-hk/trustless-sidechain/issues/681)
    outputContainsCheckpointNft :: Bool
    outputContainsCheckpointNft = Value.assetClassValueOf (txOutValue ownOutput) (get @"assetClass" checkpointParam) == 1

    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      let message =
            CheckpointMessage
              { sidechainParams = sc
              , blockHash = get @"blockHash" outputDatum
              , blockNumber = get @"blockNumber" outputDatum
              , sidechainEpoch = get @"sidechainEpoch" committeeDatum
              }
          tokenName = TokenName $ Builtins.blake2b_256 (serializeCheckpointMsg message)
          amount = Value.valueOf minted committeeCertificateVerificationCurrencySymbol tokenName
       in amount > 0

-- | 'initCheckpointMintTn'  is the token name of the NFT which identifies
-- the utxo which contains the checkpoint. We use an empty bytestring for
-- this because the name really doesn't matter, so we mighaswell save a few
-- bytes by giving it the empty name.
{-# INLINEABLE initCheckpointMintTn #-}
initCheckpointMintTn :: TokenName
initCheckpointMintTn = TokenName Builtins.emptyByteString

-- | 'initCheckpointMintAmount' is the amount of the currency to mint which
-- is 1.
{-# INLINEABLE initCheckpointMintAmount #-}
initCheckpointMintAmount :: Integer
initCheckpointMintAmount = 1

-- | 'mkCheckpointPolicy' is the minting policy for the NFT which identifies
-- the checkpoint
--
-- OnChain error descriptions:
--
--   ERROR-CHECKPOINT-POLICY-01: The transaction does not spend init token.
--
--   ERROR-CHECKPOINT-POLICY-02: Wrong amount minted.
{-# INLINEABLE mkCheckpointPolicy #-}
mkCheckpointPolicy :: InitTokenAssetClass -> BuiltinData -> ScriptContextRaw -> Bool
mkCheckpointPolicy itac _red scriptContext =
  traceIfFalse "ERROR-CHECKPOINT-POLICY-01" initTokenBurned
    && traceIfFalse "ERROR-CHECKPOINT-POLICY-02" checkMintedAmount
  where
    mint =
      PlutusTx.unsafeFromBuiltinData
        . unTxInfoMintRaw
        . safeGetTxInfoMint
        . safeGetTxInfo
        $ scriptContext

    initTokenBurned :: Bool
    initTokenBurned =
      oneTokenBurned
        mint
        (get @"initTokenCurrencySymbol" itac)
        (get @"initTokenName" itac)

    -- Assert that we have minted exactly one of this currency symbol
    checkMintedAmount :: Bool
    checkMintedAmount =
      case fmap AssocMap.toList $ AssocMap.lookup (safeOwnCurrencySymbol scriptContext) $ getValue mint of
        Just [(tn', amt)] -> tn' == initCheckpointMintTn && amt == initCheckpointMintAmount
        _ -> False

mkCheckpointPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCheckpointPolicyUntyped itac red ctx =
  check $
    mkCheckpointPolicy
      (PlutusTx.unsafeFromBuiltinData itac)
      (PlutusTx.unsafeFromBuiltinData red)
      (ScriptContextRaw ctx)

serialisableCheckpointPolicy :: Script
serialisableCheckpointPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkCheckpointPolicyUntyped||])

mkCheckpointValidatorUntyped ::
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkCheckpointValidatorUntyped checkpointParam versioningConfig datum red ctx =
  check $
    mkCheckpointValidator
      (PlutusTx.unsafeFromBuiltinData checkpointParam)
      (PlutusTx.unsafeFromBuiltinData versioningConfig)
      (PlutusTx.unsafeFromBuiltinData datum)
      red
      (PlutusTx.unsafeFromBuiltinData ctx)

serialisableCheckpointValidator :: Script
serialisableCheckpointValidator =
  fromCompiledCode $$(PlutusTx.compile [||mkCheckpointValidatorUntyped||])

{-# INLINEABLE safeOwnCurrencySymbol #-}

-- | The 'CurrencySymbol' of the current validator script.
--   Adapted from Plutus.V2.Ledger.Contexts.ownCurrencySymbol
safeOwnCurrencySymbol :: ScriptContextRaw -> CurrencySymbol
safeOwnCurrencySymbol bd = case safeGetMinting $ safeGetScriptPurpose bd of
  Just (CurrencySymbolRaw cs) -> PlutusTx.unsafeFromBuiltinData cs
  Nothing -> traceError "Lh" -- "Can't get currency symbol of the current validator script"
