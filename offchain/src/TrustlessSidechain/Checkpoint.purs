module TrustlessSidechain.Checkpoint
  ( module ExportTypes
  , module ExportUtils
  , saveCheckpoint
  , getCheckpointPolicy
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, throwContractError)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , awaitTxConfirmed
  , balanceTx
  , outputDatumDatum
  , signTransaction
  , submit
  )
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.Map as Map
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  , CheckpointParameter(CheckpointParameter)
  , CheckpointRedeemer(CheckpointRedeemer)
  , InitCheckpointMint(InitCheckpointMint)
  )
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  , InitCheckpointMint(InitCheckpointMint)
  ) as ExportTypes
import TrustlessSidechain.Checkpoint.Utils
  ( checkpointPolicy
  , checkpointValidator
  , findCheckpointUtxo
  , initCheckpointMintTn
  , normalizeSignatures
  , serialiseCheckpointMessage
  )
import TrustlessSidechain.Checkpoint.Utils
  ( checkpointPolicy
  , checkpointValidator
  , initCheckpointMintTn
  , serialiseCheckpointMessage
  ) as ExportUtils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSAggregateSignatures(Plain)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (assetClass, assetClassValue)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( findUpdateCommitteeHashUtxo
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging as Logging

saveCheckpoint ∷ CheckpointEndpointParam → Contract TransactionHash
saveCheckpoint = runSaveCheckpoint <<< normalizeSignatures

runSaveCheckpoint ∷ CheckpointEndpointParam → Contract TransactionHash
runSaveCheckpoint
  ( CheckpointEndpointParam
      { sidechainParams
      , committeeSignatures
      , newCheckpointBlockHash
      , newCheckpointBlockNumber
      , sidechainEpoch
      }
  ) = do
  let -- `mkErr` is used to help generate log messages
    mkErr = Logging.mkReport "Checkpoint" "runSaveCheckpoint"

  -- Getting the minting policy / currency symbol / token name for checkpointing
  -------------------------------------------------------------
  { checkpointCurrencySymbol, checkpointTokenName } ← getCheckpointPolicy
    sidechainParams

  { committeeOracleCurrencySymbol, committeeOracleTokenName } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sidechainParams

  when (null committeeSignatures)
    (throwContractError $ mkErr "No signatures provided")

  let
    curCommitteePubKeys /\ allCurCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures
        committeeSignatures
    _ /\ curCommitteeSignatures = Utils.Crypto.takeExactlyEnoughSignatures
      (unwrap sidechainParams).thresholdNumerator
      (unwrap sidechainParams).thresholdDenominator
      (curCommitteePubKeys /\ allCurCommitteeSignatures)

  checkpointMessage ← liftContractM (mkErr "Failed to get checkpoint message")
    $ serialiseCheckpointMessage
    $ CheckpointMessage
        { sidechainParams
        , checkpointBlockHash: newCheckpointBlockHash
        , checkpointBlockNumber: newCheckpointBlockNumber
        , sidechainEpoch
        }

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap sidechainParams).thresholdNumerator)
        ((unwrap sidechainParams).thresholdDenominator)
        curCommitteePubKeys
        checkpointMessage
        curCommitteeSignatures
    )
    ( throwContractError $ mkErr
        "Invalid committee signatures for CheckpointMessage"
    )

  -- Getting checkpoint validator
  let
    checkpointParam = CheckpointParameter
      { sidechainParams
      , checkpointAssetClass: assetClass checkpointCurrencySymbol
          checkpointTokenName
      , committeeOracleAssetClass: assetClass committeeOracleCurrencySymbol
          committeeOracleTokenName
      }
  validator ← checkpointValidator checkpointParam
  let checkpointValidatorHash = Scripts.validatorHash validator

  -- Getting the checkpoint utxo
  checkpointUtxoLookup ← findCheckpointUtxo checkpointParam
  { index: checkpointOref
  , value: checkpointTxOut
  } ←
    liftContractM (mkErr "Failed to find checkpoint UTxO") checkpointUtxoLookup

  -- Getting the validator / minting policy for the merkle root token.
  -- This is needed to get the committee hash utxo.
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    sidechainParams

  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
        , committeeOraclePolicy: committeeOracleCurrencySymbol
        }
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys

  -- TODO: the hard coded committee certificate verifications will be replaced
  -- soon in a seperate PR.
  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      committeeCertificateMint
      (Plain mempty)

  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , committeeCertificateVerificationCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    liftContractM
      (mkErr "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
      , merkleRootTokenCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol
      }

  -- Grabbing the current committee
  lkup ← findUpdateCommitteeHashUtxo uch
  { index: committeeOref
  , value:
      committeeOracleTxOut@
        (TransactionOutputWithRefScript { output: TransactionOutput tOut })
  } ←
    liftContractM (mkErr "Failed to find update committee hash UTxO") $ lkup

  comitteeHashDatum ←
    liftContractM (mkErr "Update committee hash UTxO is missing inline datum")
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← liftContractM
    (mkErr "Datum at update committee hash UTxO fromData failed")
    (fromData $ unwrap comitteeHashDatum)
  when (datum.aggregatePubKeys /= curCommitteeHash)
    (throwContractError "Incorrect committee provided")

  -- Building / submitting the transaction.
  let
    newCheckpointDatum = Datum $ toData
      ( CheckpointDatum
          { blockHash: newCheckpointBlockHash
          , blockNumber: newCheckpointBlockNumber
          }
      )
    value = assetClassValue (unwrap checkpointParam).checkpointAssetClass one
    redeemer = Redeemer $ toData
      ( CheckpointRedeemer
          { committeeSignatures: curCommitteeSignatures
          , committeePubKeys: curCommitteePubKeys
          , newCheckpointBlockHash
          , newCheckpointBlockNumber
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (Map.singleton checkpointOref checkpointTxOut)
        <> Lookups.unspentOutputs
          (Map.singleton committeeOref committeeOracleTxOut)
        <> Lookups.validator validator

    constraints = TxConstraints.mustSpendScriptOutput checkpointOref redeemer
      <> TxConstraints.mustPayToScript checkpointValidatorHash newCheckpointDatum
        DatumInline
        value
      <> TxConstraints.mustReferenceOutput committeeOref

  ubTx ← liftedE
    (lmap (show >>> mkErr) <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap (show >>> mkErr) <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (mkErr "Submitted checkpoint transaction: " <> show txId)
  awaitTxConfirmed txId
  logInfo' (mkErr "Checkpoint transaction submitted successfully")

  pure txId

-- | `getCheckpointPolicy` grabs the checkpoint hash policy, currency symbol and token name
-- | (potentially throwing an error in the case that it is not possible).
getCheckpointPolicy ∷
  SidechainParams →
  Contract
    { checkpointPolicy ∷ MintingPolicy
    , checkpointCurrencySymbol ∷ CurrencySymbol
    , checkpointTokenName ∷ TokenName
    }
getCheckpointPolicy (SidechainParams sp) = do
  let
    mkErr = Logging.mkReport "CheckpointPolicy" "getCheckpointPolicy"
  checkpointPolicy ← checkpointPolicy $
    InitCheckpointMint { icTxOutRef: sp.genesisUtxo }
  checkpointCurrencySymbol ← liftContractM
    (mkErr "Failed to get checkpoint CurrencySymbol")
    (Value.scriptCurrencySymbol checkpointPolicy)
  let checkpointTokenName = initCheckpointMintTn
  pure
    { checkpointPolicy, checkpointCurrencySymbol, checkpointTokenName }
