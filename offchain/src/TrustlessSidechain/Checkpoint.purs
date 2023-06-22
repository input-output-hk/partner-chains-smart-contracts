module TrustlessSidechain.Checkpoint
  ( module ExportTypes
  , module ExportUtils
  , saveCheckpoint
  , getCheckpointPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , outputDatumDatum
  )
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
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
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (assetClass, assetClassValue)
import TrustlessSidechain.UpdateCommitteeHash (getCommitteeHashPolicy)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( findUpdateCommitteeHashUtxo
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript, InvalidData, NotFoundUtxo)
  , OffchainError(InvalidInputError, InternalError)
  )
import TrustlessSidechain.Utils.Logging as Logging
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

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

  { committeeHashCurrencySymbol, committeeHashTokenName } ←
    getCommitteeHashPolicy sidechainParams

  when (null committeeSignatures)
    (throwContractError $ mkErr (InvalidInputError "No signatures provided"))

  let
    curCommitteePubKeys /\ allCurCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures
        committeeSignatures
    _ /\ curCommitteeSignatures = Utils.Crypto.takeExactlyEnoughSignatures
      sidechainParams
      (curCommitteePubKeys /\ allCurCommitteeSignatures)

  checkpointMessage ←
    liftContractM
      (mkErr (InternalError (InvalidData "CheckpointMessage")))
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
        (InvalidInputError "Invalid committee signatures for CheckpointMessage")

    )

  -- Getting checkpoint validator
  let
    checkpointParam = CheckpointParameter
      { sidechainParams
      , checkpointAssetClass: assetClass checkpointCurrencySymbol
          checkpointTokenName
      , committeeHashAssetClass: assetClass committeeHashCurrencySymbol
          committeeHashTokenName
      }
  validator ← checkpointValidator checkpointParam
  let checkpointValidatorHash = Scripts.validatorHash validator

  -- Getting the checkpoint utxo
  checkpointUtxoLookup ← findCheckpointUtxo checkpointParam
  { index: checkpointOref
  , value: checkpointTxOut
  } ←
    liftContractM (mkErr (InternalError (NotFoundUtxo "Checkpoint UTxO")))
      checkpointUtxoLookup

  -- Getting the validator / minting policy for the merkle root token.
  -- This is needed to get the committee hash utxo.
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    liftContractM
      (mkErr (InternalError (InvalidScript "MerkleRootTokenCurrencySymbol")))
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  let
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: assetClass committeeHashCurrencySymbol
          committeeHashTokenName
      , merkleRootTokenCurrencySymbol
      }

  -- Grabbing the current committee
  lkup ← findUpdateCommitteeHashUtxo uch
  { index: committeeOref
  , value:
      committeeHashTxOut@
        (TransactionOutputWithRefScript { output: TransactionOutput tOut })
  } ←
    liftContractM (mkErr (InternalError (NotFoundUtxo "Committee Hash UTxO")))
      lkup

  comitteeHashDatum ←
    liftContractM
      ( mkErr
          ( InternalError
              (InvalidData "Committee Hash UTxO is missing inline datum")
          )
      )
      $ outputDatumDatum tOut.datum
  UpdateCommitteeHashDatum datum ← liftContractM
    ( mkErr
        ( InternalError
            (InvalidData "Decoding datum at Committee Hash UTxO failed")
        )
    )
    (fromData $ unwrap comitteeHashDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError (InvalidInputError "Incorrect committee provided"))

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
        <> Lookups.unspentOutputs (Map.singleton committeeOref committeeHashTxOut)
        <> Lookups.validator validator

    constraints = TxConstraints.mustSpendScriptOutput checkpointOref redeemer
      <> TxConstraints.mustPayToScript checkpointValidatorHash newCheckpointDatum
        DatumInline
        value
      <> TxConstraints.mustReferenceOutput committeeOref

  balanceSignAndSubmit "Save Checkpoint" lookups constraints

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
    ( mkErr
        (InternalError (InvalidScript "Failed to get checkpoint CurrencySymbol"))
    )
    (Value.scriptCurrencySymbol checkpointPolicy)
  let checkpointTokenName = initCheckpointMintTn
  pure
    { checkpointPolicy, checkpointCurrencySymbol, checkpointTokenName }
