{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.CheckpointValidator where

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
import Plutus.V2.Ledger.Tx (OutputDatum (..))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Prelude as PlutusTx
import TrustlessSidechain.Types (
  -- CheckpointDatum (checkpointBlockHash, checkpointBlockNumber),
  CheckpointDatum,
  CheckpointMessage (CheckpointMessage, checkpointMsgBlockHash, checkpointMsgBlockNumber, checkpointMsgSidechainEpoch, checkpointMsgSidechainParams),
  CheckpointParameter (checkpointSidechainParams, checkpointToken),
  CheckpointRedeemer (checkpointCommitteePubKeys, checkpointCommitteeSignatures, newCheckpointBlockHash, newCheckpointBlockNumber),
  SidechainParams (
    thresholdDenominator,
    thresholdNumerator
  ),
  SidechainPubKey (getSidechainPubKey),
  -- SignedMerkleRootMint (..),
  UpdateCommitteeHashDatum (committeeHash, sidechainEpoch)
 )

-- import TrustlessSidechain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.Utils (verifyMultisig)
import Prelude qualified

{- | 'serialiseUchm' serialises a 'CheckpointMessage' via converting
 to the Plutus data representation, then encoding it to cbor via the builtin.
-}
serializeCheckpointMsg :: CheckpointMessage -> BuiltinByteString
serializeCheckpointMsg = Builtins.serialiseData . IsData.toBuiltinData

{- | 'aggregateKeys' aggregates a list of public keys into a singCheckpointl
 committee hash by essentially computing the merkle root of all public keys
 together.
 We call the output of this function an /aggregate public key/.
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [SidechainPubKey] -> BuiltinByteString
aggregateKeys = Builtins.blake2b_256 . mconcat . map getSidechainPubKey

{- Note [Aggregate Keys Append Scheme]
 Potential optimizations: instead of doing the concatenated hash, we could
 instead compute a merkle root.
 -}

{- | 'aggregateCheck' takes a sequence of public keys and an aggregate public
 key, and returns true or false to determinig whether the public keys weCheckpointr
 used to produce the aggregate public key
-}
{-# INLINEABLE aggregateCheck #-}
aggregateCheck :: [SidechainPubKey] -> BuiltinByteString -> Bool
aggregateCheck pubKeys avk = aggregateKeys pubKeys == avk

{-# INLINEABLE mkCheckpointValidator #-}
mkCheckpointValidator ::
  CheckpointParameter ->
  CheckpointDatum ->
  CheckpointRedeemer ->
  ScriptContext ->
  Bool
mkCheckpointValidator checkpointParam _ red ctx =
  traceIfFalse "error 'mkCheckpointValidator': output missing NFT" outputHasToken
    && traceIfFalse "error 'mkCheckpointValidator': committee signature invalid" signedByCurrentCommittee
    && traceIfFalse "error 'mkCheckpointValidator': current committee mismatch" isCurrentCommittee
  where
    -- && traceIfFalse
    --   "error 'mkCheckpointValidator': expected different new committee"
    --   (checkpointCommitteeHash outputDatum == aggregateKeys (newCommitteePubKeys red))
    -- Note: we only need to check if the new committee is "as signed
    -- by the committee", since we already know that the sidechainEpoch in
    -- the datum was "as signed by the committee" -- see how we constructed
    -- the 'CheckpointMessage'
    -- && traceIfFalse
    --   "error 'mkCheckpointValidator': sidechain epoch is not strictly increasing"
    --   (sidechainEpoch dat < sidechainEpoch outputDatum)

    info :: TxInfo
    info = scriptContextTxInfo ctx

    sc :: SidechainParams
    sc = checkpointSidechainParams checkpointParam

    committeeDatum :: UpdateCommitteeHashDatum
    committeeDatum =
      let go :: [TxInInfo] -> UpdateCommitteeHashDatum
          go (t : ts)
            | o <- txInInfoResolved t
              , -- See Note [Committee Hash Inline Datum] in
                -- 'TrustlessSidechain.UpdateCommitteeHash'
                OutputDatum d <- txOutDatum o =
              IsData.unsafeFromBuiltinData $ getDatum d
            | otherwise = go ts
          go [] = traceError "error 'CheckpointValidator' no committee utxo given as reference input"
       in go (txInfoReferenceInputs info)

    ownOutput :: TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "Expected exactly one output"

    -- outputDatum :: CheckpointDatum
    -- outputDatum = case txOutDatum ownOutput of
    --   OutputDatum d -> IsData.unsafeFromBuiltinData (getDatum d)
    --   _ -> traceError "error 'mkCheckpointValidator': no output inline datum missing"

    outputHasToken :: Bool
    outputHasToken = hasNft (txOutValue ownOutput)

    hasNft :: Value -> Bool
    hasNft val = Value.assetClassValueOf val (checkpointToken checkpointParam) == 1

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
              , checkpointMsgBlockHash = newCheckpointBlockHash red
              , checkpointMsgBlockNumber = newCheckpointBlockNumber red
              , checkpointMsgSidechainEpoch = sidechainEpoch committeeDatum
              }
       in verifyMultisig
            (getSidechainPubKey <$> checkpointCommitteePubKeys red)
            threshold
            (Builtins.blake2b_256 (serializeCheckpointMsg message))
            (checkpointCommitteeSignatures red)

    isCurrentCommittee :: Bool
    isCurrentCommittee = aggregateCheck (checkpointCommitteePubKeys red) $ committeeHash committeeDatum

-- * Initializing the committee hash

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
    checkMintedAmount = case fmap AssocMap.toList $ AssocMap.lookup (Contexts.ownCurrencySymbol ctx) $ getValue $ txInfoMint info of
      Just [(tn', amt)] -> tn' == initCheckpointMintTn && amt == initCheckpointMintAmount
      _ -> False

-- CTL hack
mkCheckpointPolicyUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCheckpointPolicyUntyped = ScriptUtils.mkUntypedMintingPolicy . mkCheckpointPolicy . PlutusTx.unsafeFromBuiltinData

serialisableCheckpointPolicy :: Versioned Ledger.Script
serialisableCheckpointPolicy = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCheckpointPolicyUntyped||])) PlutusV2

mkCheckpointValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkCheckpointValidatorUntyped = ScriptUtils.mkUntypedValidator . mkCheckpointValidator . PlutusTx.unsafeFromBuiltinData

serialisableCheckpointValidator :: Versioned Ledger.Script
serialisableCheckpointValidator = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||mkCheckpointValidatorUntyped||])) PlutusV2
