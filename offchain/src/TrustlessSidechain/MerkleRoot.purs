-- | `MerkleRoot` contains the endpoint functionality for the `MerkleRoot` endpoint
module TrustlessSidechain.MerkleRoot
  ( module ExportTypes
  , module ExportUtils
  , saveRoot
  , getMerkleRootTokenMintingPolicy
  ) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , liftContractM
  , liftedM
  , throwContractError
  )
import Contract.PlutusData (fromData, toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , outputDatumDatum
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSAggregateSignatures(Plain)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeOraclePolicy
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  ) as ExportTypes
import TrustlessSidechain.MerkleRoot.Utils
  ( findMerkleRootTokenUtxo
  , merkleRootTokenMintingPolicy
  , merkleRootTokenValidator
  , serialiseMrimHash
  ) as ExportUtils
import TrustlessSidechain.MerkleRoot.Utils
  ( findPreviousMerkleRootTokenUtxo
  , merkleRootTokenMintingPolicy
  , merkleRootTokenValidator
  , normalizeSaveRootParams
  , serialiseMrimHash
  )
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript, NotFoundUtxo, InvalidData)
  , OffchainError(InvalidInputError, InternalError)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

-- | `getMerkleRootTokenMintingPolicy` creates the `SignedMerkleRootMint`
-- | parameter from the given sidechain parameters
getMerkleRootTokenMintingPolicy ∷
  SidechainParams →
  Contract
    { merkleRootTokenMintingPolicy ∷ MintingPolicy
    , merkleRootTokenCurrencySymbol ∷ CurrencySymbol
    }
getMerkleRootTokenMintingPolicy sidechainParams = do
  merkleRootValidatorHash ← map Scripts.validatorHash $ merkleRootTokenValidator
    sidechainParams

  updateCommitteeHashPolicy ← CommitteeOraclePolicy.committeeOraclePolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      ( show
          ( InternalError
              (InvalidScript "CommitteeOraclePolicy")
          )
      )
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  policy ← merkleRootTokenMintingPolicy $ SignedMerkleRootMint
    { sidechainParams
    , updateCommitteeHashCurrencySymbol
    , merkleRootValidatorHash
    }
  merkleRootTokenCurrencySymbol ←
    liftContractM (show (InternalError (InvalidScript "MerkleRootPolicy"))) $
      Value.scriptCurrencySymbol policy
  pure $ { merkleRootTokenMintingPolicy: policy, merkleRootTokenCurrencySymbol }

-- | `saveRoot` is the endpoint.
saveRoot ∷ SaveRootParams → Contract TransactionHash
saveRoot = runSaveRoot <<< normalizeSaveRootParams

-- | `runSaveRoot` is the main worker of the `saveRoot` endpoint.
-- | Preconditions
-- |    - `SaveRootParams` must be normalized with `MerkleRoot.Utils.normalizeSaveRootParams`
runSaveRoot ∷ SaveRootParams → Contract TransactionHash
runSaveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, previousMerkleRoot, committeeSignatures }
  ) = do

  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  updateCommitteeHashPolicy ← CommitteeOraclePolicy.committeeOraclePolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      ( show
          ( InternalError
              (InvalidScript "CommitteeHashPolicy")
          )
      )
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy

  merkleRootValidatorHash ← map Scripts.validatorHash $ merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol
      , merkleRootValidatorHash
      }
  rootTokenMP ← merkleRootTokenMintingPolicy smrm
  rootTokenCS ←
    liftContractM
      (show (InternalError (InvalidScript "MerkleRootTokenMintingPolicy")))
      $ Value.scriptCurrencySymbol rootTokenMP
  rootTokenVal ← merkleRootTokenValidator sidechainParams
  merkleRootTokenName ←
    liftContractM
      ( show
          ( InternalError
              ( InvalidData
                  "Invalid Merkle root TokenName for merkleRootTokenMintingPolicy"
              )
          )
      )
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
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
        , committeeOraclePolicy: updateCommitteeHashCurrencySymbol
        }
  -- TODO: this is going to get all replaced soon
  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      committeeCertificateMint
      (Plain mempty)
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: updateCommitteeHashCurrencySymbol
      , merkleRootTokenCurrencySymbol: rootTokenCS
      , committeeCertificateVerificationCurrencySymbol
      }
  { index: committeeOracleTxIn
  , value: committeeOracleTxOut
  } ←
    liftedM
      (show (InternalError (NotFoundUtxo "Failed to find committee hash utxo")))
      $
        UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  let
    committeePubKeys /\ allSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures committeeSignatures
    _ /\ signatures = Utils.Crypto.takeExactlyEnoughSignatures
      (unwrap sidechainParams).thresholdNumerator
      (unwrap sidechainParams).thresholdDenominator
      (committeePubKeys /\ allSignatures)

  -- Verifying the signature is valid
  void do
    mrimHash ←
      liftContractM
        ( show
            ( InternalError
                (InvalidData "Failed to create MerkleRootInsertionMessage")
            )
        )
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
      $ InvalidInputError
          "Invalid committee signatures for MerkleRootInsertionMessage"

  -- Verifying the provided committee is actually the committee stored on chain
  void do
    let
      TransactionOutputWithRefScript { output: TransactionOutput tOut } =
        committeeOracleTxOut

      committeeHash = Utils.Crypto.aggregateKeys committeePubKeys
    rawDatum ←
      liftContractM
        ( show
            ( InternalError
                (NotFoundUtxo "Committee hash UTxO is missing inline datum")
            )
        )
        $ outputDatumDatum tOut.datum
    UpdateCommitteeDatum datum ← liftContractM
      ( show
          ( InternalError
              (InvalidData "Decoding datum at committee hash UTxO failed")
          )
      )
      (fromData $ unwrap rawDatum)

    when (datum.aggregatePubKeys /= committeeHash)
      $ throwContractError (InvalidInputError "Incorrect committee provided")

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
        <> TxConstraints.mustReferenceOutput committeeOracleTxIn
        <> case maybePreviousMerkleRootUtxo of
          Nothing → mempty
          Just { index: oref } → TxConstraints.mustReferenceOutput oref

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy rootTokenMP
      <> Lookups.unspentOutputs
        (Map.singleton committeeOracleTxIn committeeOracleTxOut)
      <> case maybePreviousMerkleRootUtxo of
        Nothing → mempty
        Just { index: txORef, value: txOut } → Lookups.unspentOutputs
          (Map.singleton txORef txOut)

  balanceSignAndSubmit "Save Merkle root" lookups constraints
