-- | `MPTRoot` contains the endpoint functionality for the `MPTRoot` endpoint
module MPTRoot
  ( module MPTRoot.Types
  , module MPTRoot.Utils
  , saveRoot
  , getMptRootTokenMintingPolicy
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
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.Map as Map
import MPTRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  )
import MPTRoot.Utils
  ( findMptRootTokenUtxo
  , findPreviousMptRootTokenUtxo
  , mptRootTokenMintingPolicy
  , mptRootTokenValidator
  , normalizeSaveRootParams
  , serialiseMrimHash
  )
import SidechainParams (SidechainParams)
import SidechainParams as SidechainParams
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | `getMptRootTokenMintingPolicy` creates the `SignedMerkleRootMint`
-- | parameter from the given sidechain parameters, and calls
-- | `mptRootTokenValidator`
getMptRootTokenMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
getMptRootTokenMintingPolicy sidechainParams = do
  let msg = report "getMptRootTokenMintingPolicy"
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      (msg "Failed to get updateCommitteeHash CurrencySymbol")
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  mptRootTokenMintingPolicy $ SignedMerkleRootMint
    { sidechainParams
    , updateCommitteeHashCurrencySymbol
    }

-- | `saveRoot` is the endpoint.
saveRoot ∷ SaveRootParams → Contract () TransactionHash
saveRoot = runSaveRoot <<< normalizeSaveRootParams

-- | `runSaveRoot` is the main worker of the `saveRoot` endpoint.
-- | Preconditions
-- |    - `SaveRootParams` must be normalized with `MPTRoot.Utils.normalizeSaveRootParams`
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
  rootTokenMP ← mptRootTokenMintingPolicy smrm
  rootTokenCS ←
    liftContractM (msg "Cannot get CurrencySymbol of mptRootTokenMintingPolicy")
      $ Value.scriptCurrencySymbol rootTokenMP
  rootTokenVal ← mptRootTokenValidator sidechainParams
  merkleRootTokenName ←
    liftContractM
      (msg "Invalid merkle root TokenName for mptRootTokenMintingPolicy")
      $ Value.mkTokenName merkleRoot

  -- Grab the transaction holding the last merkle root
  ---------------------------------------------------------
  maybePreviousMerkleRootUtxo ← findPreviousMptRootTokenUtxo previousMerkleRoot
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
      , mptRootTokenCurrencySymbol: rootTokenCS
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
          { sidechainParams: SidechainParams.convertSCParams sidechainParams
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
report = Utils.Logging.mkReport <<< { mod: "MPTRoot", fun: _ }
