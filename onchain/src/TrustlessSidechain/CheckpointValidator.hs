{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TrustlessSidechain.CheckpointValidator (
  InitCheckpointMint (..),
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

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Typed.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (
  Datum (getDatum),
  TokenName (TokenName),
  Value (getValue),
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (scriptContextTxInfo),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
  TxOutRef,
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import Plutus.V2.Ledger.Tx (OutputDatum (OutputDatum))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Prelude as PlutusPrelude
import TrustlessSidechain.Types (
  CheckpointDatum (
    checkpointBlockHash,
    checkpointBlockNumber
  ),
  CheckpointMessage (
    CheckpointMessage,
    checkpointMsgBlockHash,
    checkpointMsgBlockNumber,
    checkpointMsgSidechainEpoch,
    checkpointMsgSidechainParams
  ),
  CheckpointParameter (
    checkpointAssetClass,
    checkpointSidechainParams,
    committeeHashAssetClass
  ),
  CheckpointRedeemer (
    checkpointCommitteePubKeys,
    checkpointCommitteeSignatures
  ),
  SidechainParams (
    thresholdDenominator,
    thresholdNumerator
  ),
  SidechainPubKey (getSidechainPubKey),
  UpdateCommitteeHashDatum (committeeHash, sidechainEpoch),
 )
import TrustlessSidechain.Utils (aggregateCheck, verifyMultisig)
import Prelude qualified

serializeCheckpointMsg :: CheckpointMessage -> BuiltinByteString
serializeCheckpointMsg = Builtins.serialiseData . IsData.toBuiltinData

{-# INLINEABLE mkCheckpointValidator #-}
mkCheckpointValidator ::
  CheckpointParameter ->
  CheckpointDatum ->
  CheckpointRedeemer ->
  ScriptContext ->
  Bool
mkCheckpointValidator checkpointParam datum red ctx =
  traceIfFalse "error 'mkCheckpointValidator': output missing NFT" outputContainsCheckpointNft
    && traceIfFalse "error 'mkCheckpointValidator': committee signature invalid" signedByCurrentCommittee
    && traceIfFalse "error 'mkCheckpointValidator': current committee mismatch" isCurrentCommittee
    && traceIfFalse
      "error 'mkCheckpointValidator' new checkpoint block number must be greater than current checkpoint block number"
      (checkpointBlockNumber datum < checkpointBlockNumber outputDatum)
    && traceIfFalse
      "error 'mkCheckpointValidator' new checkpoint block hash must be different from current checkpoint block hash"
      (checkpointBlockHash datum /= checkpointBlockHash outputDatum)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sc :: SidechainParams
    sc = checkpointSidechainParams checkpointParam

    -- Check if the transaction input value contains the current committee NFT
    containsCommitteeNft :: TxInInfo -> Bool
    containsCommitteeNft txIn =
      let resolvedOutput = txInInfoResolved txIn
          outputValue = txOutValue resolvedOutput
       in Value.assetClassValueOf outputValue (committeeHashAssetClass checkpointParam) == 1

    -- Extract the UpdateCommitteeHashDatum from the list of input transactions
    extractCommitteeDatum :: [TxInInfo] -> UpdateCommitteeHashDatum
    extractCommitteeDatum [] = traceError "error 'CheckpointValidator' no committee utxo given as reference input"
    extractCommitteeDatum (txIn : txIns)
      | containsCommitteeNft txIn = case txOutDatum (txInInfoResolved txIn) of
        OutputDatum d -> IsData.unsafeFromBuiltinData $ getDatum d
        _ -> extractCommitteeDatum txIns
      | otherwise = extractCommitteeDatum txIns

    committeeDatum :: UpdateCommitteeHashDatum
    committeeDatum = extractCommitteeDatum (txInfoReferenceInputs info)

    ownOutput :: TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "Expected exactly one output"

    outputDatum :: CheckpointDatum
    outputDatum = case txOutDatum ownOutput of
      OutputDatum d -> IsData.unsafeFromBuiltinData (getDatum d)
      _ -> traceError "error 'mkCheckpointValidator': no output inline datum missing"

    outputContainsCheckpointNft :: Bool
    outputContainsCheckpointNft = Value.assetClassValueOf (txOutValue ownOutput) (checkpointAssetClass checkpointParam) == 1

    threshold :: Integer
    threshold =
      ( length (checkpointCommitteePubKeys red)
          `Builtins.multiplyInteger` thresholdNumerator sc
          `Builtins.divideInteger` thresholdDenominator sc
      )
        + 1

    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      let message =
            CheckpointMessage
              { checkpointMsgSidechainParams = sc
              , checkpointMsgBlockHash = checkpointBlockHash outputDatum
              , checkpointMsgBlockNumber = checkpointBlockNumber outputDatum
              , checkpointMsgSidechainEpoch = sidechainEpoch committeeDatum
              }
       in verifyMultisig
            (getSidechainPubKey <$> checkpointCommitteePubKeys red)
            threshold
            (Builtins.blake2b_256 (serializeCheckpointMsg message))
            (checkpointCommitteeSignatures red)

    isCurrentCommittee :: Bool
    isCurrentCommittee = aggregateCheck (checkpointCommitteePubKeys red) $ committeeHash committeeDatum

-- | 'InitCheckpointMint' is used as the parameter for the minting policy
newtype InitCheckpointMint = InitCheckpointMint
  { -- | 'TxOutRef' is the output reference to mint the NFT initially.
    icTxOutRef :: TxOutRef
  }
  deriving newtype (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic, PlutusTx.UnsafeFromData)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''InitCheckpointMint

{- | 'initCheckpointMintTn'  is the token name of the NFT which identifies
 the utxo which contains the checkpoint. We use an empty bytestring for
 this because the name really doesn't matter, so we mighaswell save a few
 bytes by giving it the empty name.
-}
{-# INLINEABLE initCheckpointMintTn #-}
initCheckpointMintTn :: TokenName
initCheckpointMintTn = TokenName Builtins.emptyByteString

{- | 'initCheckpointMintAmount' is the amount of the currency to mint which
 is 1.
-}
{-# INLINEABLE initCheckpointMintAmount #-}
initCheckpointMintAmount :: Integer
initCheckpointMintAmount = 1

{- | 'mkCheckpointPolicy' is the minting policy for the NFT which identifies
 the checkpoint
-}
{-# INLINEABLE mkCheckpointPolicy #-}
mkCheckpointPolicy :: InitCheckpointMint -> () -> ScriptContext -> Bool
mkCheckpointPolicy ichm _red ctx =
  traceIfFalse "error 'mkCheckpointPolicy' UTxO not consumed" hasUtxo
    && traceIfFalse "error 'mkCheckpointPolicy' wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oref :: TxOutRef
    oref = icTxOutRef ichm

    hasUtxo :: Bool
    hasUtxo = any ((oref ==) . txInInfoOutRef) $ txInfoInputs info

    -- Assert that we have minted exactly one of this currency symbol
    checkMintedAmount :: Bool
    checkMintedAmount =
      case fmap AssocMap.toList $ AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue $ txInfoMint info of
        Just [(tn', amt)] -> tn' == initCheckpointMintTn && amt == initCheckpointMintAmount
        _ -> False

-- CTL hack
mkCheckpointPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCheckpointPolicyUntyped =
  ScriptUtils.mkUntypedMintingPolicy
    . mkCheckpointPolicy
    . PlutusTx.unsafeFromBuiltinData

serialisableCheckpointPolicy :: Versioned Ledger.Script
serialisableCheckpointPolicy =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCheckpointPolicyUntyped||])) PlutusV2

mkCheckpointValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCheckpointValidatorUntyped =
  ScriptUtils.mkUntypedValidator
    . mkCheckpointValidator
    . PlutusTx.unsafeFromBuiltinData

serialisableCheckpointValidator :: Versioned Ledger.Script
serialisableCheckpointValidator =
  Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCheckpointValidatorUntyped||])) PlutusV2
