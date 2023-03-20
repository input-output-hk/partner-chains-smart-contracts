module TrustlessSidechain.Checkpoint
  ( module TrustlessSidechain.Checkpoint.Types
  , module TrustlessSidechain.Checkpoint.Utils
  , saveCheckpoint
  , getCheckpointPolicy
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, throwContractError)
import Contract.PlutusData (Datum(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(..)
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceTx
  , outputDatumDatum
  , signTransaction
  , submit
  )
import Contract.TxConstraints (DatumPresence(..))
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
import TrustlessSidechain.Checkpoint.Utils
  ( checkpointAssetClass
  , checkpointPolicy
  , checkpointValidator
  , findCheckpointUtxo
  , initCheckpointMintTn
  , normalizeSignatures
  , serialiseCheckpointMessage
  )
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.SidechainParams (SidechainParams(..))
import TrustlessSidechain.Types (assetClass, assetClassValue)
import TrustlessSidechain.UpdateCommitteeHash
  ( getCommitteeHashPolicy
  )
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils (findUpdateCommitteeHashUtxo)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging (class Display)
import TrustlessSidechain.Utils.Logging as Logging
import TrustlessSidechain.Utils.Logging as Utils.Logging

saveCheckpoint ∷ CheckpointEndpointParam → Contract () TransactionHash
saveCheckpoint = runSaveCheckpoint <<< normalizeSignatures

runSaveCheckpoint ∷ CheckpointEndpointParam → Contract () TransactionHash
runSaveCheckpoint params = do
  let -- `msg` is used to help generate log messages
    msg = report "runSaveCheckpoint"

  -- Getting the minting policy / currency symbol / token name for checkpointing
  -------------------------------------------------------------
  { checkpointCurrencySymbol, checkpointTokenName } ← getCheckpointPolicy
    (unwrap params).sidechainParams

  when (null (unwrap params).committeeSignatures)
    (throwContractError $ msg "No signatures provided")

  let
    curCommitteePubKeys /\ curCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures
        (unwrap params).committeeSignatures

  checkpointMessage ← liftContractM (msg "Failed to get checkpoint message")
    $ serialiseCheckpointMessage
    $ CheckpointMessage
        { sidechainParams: (unwrap params).sidechainParams
        , checkpointBlockHash: (unwrap params).newCheckpointBlockHash
        , checkpointBlockNumber: (unwrap params).newCheckpointBlockNumber
        , sidechainEpoch: (unwrap params).sidechainEpoch
        }

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap (unwrap params).sidechainParams).thresholdNumerator)
        ((unwrap (unwrap params).sidechainParams).thresholdDenominator)
        curCommitteePubKeys
        checkpointMessage
        curCommitteeSignatures
    )
    ( throwContractError $ msg
        "Invalid committee signatures for CheckpointMessage"
    )

  -- Getting checkpoint validator
  let
    checkpointParam = CheckpointParameter
      { sidechainParams: (unwrap params).sidechainParams
      , checkpointToken: assetClass checkpointCurrencySymbol checkpointTokenName
      }
  validator ← checkpointValidator checkpointParam
  let checkpointValidatorHash = Scripts.validatorHash validator

  -- Getting the checkpoint utxo
  -------------------------------------------------------------
  checkpointUtxoLookup ← findCheckpointUtxo checkpointParam
  { index: checkpointOref
  , value: checkpointTxOut
  } ←
    liftContractM (msg "Failed to find checkpoint UTxO") checkpointUtxoLookup

  -- Grabbing the committee hash UTxO
  -------------------------------------------------------------
  { committeeHashCurrencySymbol, committeeHashTokenName } ←
    getCommitteeHashPolicy (unwrap params).sidechainParams

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    (unwrap params).sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams: (unwrap params).sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    liftContractM
      (msg "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  let
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys
    uch = UpdateCommitteeHash
      { sidechainParams: (unwrap params).sidechainParams
      , uchAssetClass: assetClass committeeHashCurrencySymbol
          committeeHashTokenName
      , merkleRootTokenCurrencySymbol
      }

  -- Grabbing the old committee / verifying that it really is the old committee
  -------------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo uch
  { index: committeeOref
  , value:
      committeeHashTxOut@
        (TransactionOutputWithRefScript { output: TransactionOutput tOut })
  } ←
    liftContractM (msg "Failed to find update committee hash UTxO") $ lkup

  comitteeHashDatum ←
    liftContractM (msg "Update committee hash UTxO is missing inline datum")
      $ outputDatumDatum tOut.datum
  UpdateCommitteeHashDatum datum ← liftContractM
    (msg "Datum at update committee hash UTxO fromData failed")
    (fromData $ unwrap comitteeHashDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError "Incorrect committee provided")

  -- Building / submitting the transaction.
  -------------------------------------------------------------
  let
    newCheckpointDatum = Datum $ toData
      ( CheckpointDatum
          { blockHash: (unwrap params).newCheckpointBlockHash
          , blockNumber: (unwrap params).newCheckpointBlockNumber
          }
      )
    value = assetClassValue (unwrap checkpointParam).checkpointToken one
    redeemer = Redeemer $ toData
      ( CheckpointRedeemer
          { committeeSignatures: curCommitteeSignatures
          , committeePubKeys: curCommitteePubKeys
          , newCheckpointBlockHash: (unwrap params).newCheckpointBlockHash
          , newCheckpointBlockNumber: (unwrap params).newCheckpointBlockNumber
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (Map.singleton checkpointOref checkpointTxOut)
        <> Lookups.unspentOutputs (Map.singleton committeeOref committeeHashTxOut)
        <> Lookups.validator validator

    constraints = TxConstraints.mustSpendScriptOutput checkpointOref redeemer
      <> TxConstraints.mustPayToScript checkpointValidatorHash newCheckpointDatum
        DatumInline
        value
      <> TxConstraints.mustReferenceOutput committeeOref

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (msg "Submitted checkpoint transaction: " <> show txId)
  awaitTxConfirmed txId
  logInfo' (msg "Checkpoint transaction submitted successfully")

  pure txId

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "Checkpoint", fun: _ }

-- | `getCheckpointPolicy` grabs the checkpoint hash policy, currency symbol and token name
-- | (potentially throwing an error in the case that it is not possible).
getCheckpointPolicy ∷
  SidechainParams →
  Contract ()
    { checkpointPolicy ∷ MintingPolicy
    , checkpointCurrencySymbol ∷ CurrencySymbol
    , checkpointTokenName ∷ TokenName
    }
getCheckpointPolicy (SidechainParams sp) = do
  let
    msg = Logging.mkReport
      { mod: "CheckpointPolicy", fun: "getCheckpointPolicy" }
  checkpointPolicy ← checkpointPolicy $
    InitCheckpointMint { icTxOutRef: sp.genesisUtxo }
  checkpointCurrencySymbol ← liftContractM
    (msg "Failed to get checkpoint CurrencySymbol")
    (Value.scriptCurrencySymbol checkpointPolicy)
  let checkpointTokenName = initCheckpointMintTn
  pure
    { checkpointPolicy, checkpointCurrencySymbol, checkpointTokenName }
