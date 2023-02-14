-- | `MerkleRoot` contains the endpoint functionality for the `MerkleRoot` endpoint
module MerkleRoot
  ( module MerkleRoot.Types
  , module MerkleRoot.Utils
  , saveRoot
  , getMerkleRootTokenMintingPolicy
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , throwContractError
  )
import Contract.PlutusData (fromData, toData, unitDatum)
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
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.Map as Map
import MerkleRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  )
import MerkleRoot.Utils
  ( findMerkleRootTokenUtxo
  , findPreviousMerkleRootTokenUtxo
  , merkleRootTokenMintingPolicy
  , merkleRootTokenValidator
  , normalizeSaveRootParams
  , serialiseMrimHash
  )
import MerkleTree as MerkleTree
import SidechainParams (SidechainParams)
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | `getMerkleRootTokenMintingPolicy` creates the `SignedMerkleRootMint`
-- | parameter from the given sidechain parameters, and calls
-- | `merkleRootTokenValidator`
getMerkleRootTokenMintingPolicy ∷
  SidechainParams →
  Contract ()
    { merkleRootTokenMintingPolicy ∷ MintingPolicy
    , merkleRootTokenCurrencySymbol ∷ CurrencySymbol
    }
getMerkleRootTokenMintingPolicy sidechainParams = do
  let msg = report "getMerkleRootTokenMintingPolicy"
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      (msg "Failed to get updateCommitteeHash CurrencySymbol")
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  policy ← merkleRootTokenMintingPolicy $ SignedMerkleRootMint
    { sidechainParams
    , updateCommitteeHashCurrencySymbol
    }
  merkleRootTokenCurrencySymbol ←
    liftContractM (msg "Cannot get currency symbol") $
      Value.scriptCurrencySymbol policy
  pure $ { merkleRootTokenMintingPolicy: policy, merkleRootTokenCurrencySymbol }

-- | `saveRoot` is the endpoint.
saveRoot ∷ SaveRootParams → Contract () TransactionHash
saveRoot = runSaveRoot <<< normalizeSaveRootParams

-- | `runSaveRoot` is the main worker of the `saveRoot` endpoint.
-- | Preconditions
-- |    - `SaveRootParams` must be normalized with `MerkleRoot.Utils.normalizeSaveRootParams`
runSaveRoot ∷ SaveRootParams → Contract () TransactionHash
runSaveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, previousMerkleRoot, committeeSignatures }
  ) = do
  let msg = report "runSaveRoot"

  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      (msg "Failed to get updateCommitteeHash CurrencySymbol")
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol
      }
  rootTokenMP ← merkleRootTokenMintingPolicy smrm
  rootTokenCS ←
    liftContractM
      (msg "Cannot get CurrencySymbol of merkleRootTokenMintingPolicy")
      $ Value.scriptCurrencySymbol rootTokenMP
  rootTokenVal ← merkleRootTokenValidator sidechainParams
  merkleRootTokenName ←
    liftContractM
      (msg "Invalid merkle root TokenName for merkleRootTokenMintingPolicy")
      $ Value.mkTokenName
      $ MerkleTree.unRootHash merkleRoot

  -- Grab the transaction holding the last merkle root
  ---------------------------------------------------------
  maybePreviousMerkleRootUtxo ← findPreviousMerkleRootTokenUtxo
    previousMerkleRoot
    smrm

  -- Grab the utxo with the current committee hash / verifying that this
  -- committee has signed the current merkle root (for better error messages)
  -- verifying that this really is the old committee
  ---------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: updateCommitteeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      , merkleRootTokenCurrencySymbol: rootTokenCS
      }
  { index: committeeHashTxIn
  , value: committeeHashTxOut
  } ←
    liftedM (msg "Failed to find committee hash utxo") $
      UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  let
    committeePubKeys /\ signatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures committeeSignatures

  -- Verifying the signature is valid
  void do
    mrimHash ← liftContractM (msg "Failed to create MerkleRootInsertionMessage")
      $ serialiseMrimHash
      $ MerkleRootInsertionMessage
          { sidechainParams
          , merkleRoot
          , previousMerkleRoot
          }
    unless
      ( Utils.Crypto.verifyMultiSignature
          ((unwrap sidechainParams).thresholdNumerator)
          ((unwrap sidechainParams).thresholdDenominator)
          committeePubKeys
          mrimHash
          signatures
      )
      $ throwContractError
      $ msg "Invalid committee signatures for MerkleRootInsertionMessage"

  -- Verifying the provided committee is actually the committee stored on chain
  void do
    let
      TransactionOutputWithRefScript { output: TransactionOutput tOut } =
        committeeHashTxOut

      committeeHash = UpdateCommitteeHash.aggregateKeys committeePubKeys
    rawDatum ←
      liftContractM (msg "Update committee hash UTxO is missing inline datum")
        $ outputDatumDatum tOut.datum
    UpdateCommitteeHashDatum datum ← liftContractM
      (msg "Datum at update committee hash UTxO fromData failed")
      (fromData $ unwrap rawDatum)

    when (datum.committeeHash /= committeeHash)
      $ throwContractError "Incorrect committee provided"

  -- Building the transaction
  ---------------------------------------------------------
  let
    value = Value.singleton rootTokenCS merkleRootTokenName one

    redeemer = SignedMerkleRoot
      { merkleRoot, previousMerkleRoot, signatures, committeePubKeys }

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer (wrap (toData redeemer)) value
        <> TxConstraints.mustPayToScript (Scripts.validatorHash rootTokenVal)
          unitDatum
          TxConstraints.DatumWitness
          value
        <> TxConstraints.mustReferenceOutput committeeHashTxIn
        <> case maybePreviousMerkleRootUtxo of
          Nothing → mempty
          Just { index: oref } → TxConstraints.mustReferenceOutput oref

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy rootTokenMP
      <> Lookups.unspentOutputs
        (Map.singleton committeeHashTxIn committeeHashTxOut)
      <> case maybePreviousMerkleRootUtxo of
        Nothing → mempty
        Just { index: txORef, value: txOut } → Lookups.unspentOutputs
          (Map.singleton txORef txOut)

  -- Submitting the transaction
  ---------------------------------------------------------
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (msg ("Submitted save root Tx: " <> show txId))
  awaitTxConfirmed txId
  logInfo' (msg "Save root Tx submitted successfully!")

  pure txId

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "MerkleRoot", fun: _ }
