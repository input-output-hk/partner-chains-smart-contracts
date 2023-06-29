module TrustlessSidechain.UpdateCommitteeHash
  ( module ExportTypes
  , module ExportUtils
  , updateCommitteeHash
  , getCommitteeHashPolicy
  , findUpdateCommitteeHashUtxoFromSidechainParams
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, liftedM, throwContractError)
import Contract.PlutusData (Datum(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutput(..)
  , TransactionOutputWithRefScript(..)
  , outputDatumDatum
  )
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.Map as Map
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (assetClass, assetClassValue)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  ) as ExportTypes
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( committeeHashPolicy
  , findUpdateCommitteeHashUtxo
  , initCommitteeHashMintTn
  , normalizeCommitteeHashParams
  , serialiseUchmHash
  , updateCommitteeHashValidator
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( committeeHashPolicy
  , findUpdateCommitteeHashUtxo
  , initCommitteeHashMintTn
  , serialiseUchmHash
  , updateCommitteeHashValidator
  ) as ExportUtils
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging
  ( InternalError(ConversionError, InvalidScript, NotFoundUtxo)
  , OffchainError(InternalError, InvalidInputError)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

-- | `updateCommitteeHash` is the endpoint to submit the transaction to update
-- | the committee hash.
updateCommitteeHash ∷ UpdateCommitteeHashParams → Contract TransactionHash
updateCommitteeHash = runUpdateCommitteeHash <<< normalizeCommitteeHashParams

-- | `runUpdateCommitteeHash` is the main worker function of the
-- | `updateCommitteeHash` endpoint.
-- | Preconditions:
-- |    - `UpdateCommitteeHashParams` has been normalized via
-- |    `UpdateCommitteeHash.Utils.normalizeCommitteeHashParams`
runUpdateCommitteeHash ∷ UpdateCommitteeHashParams → Contract TransactionHash
runUpdateCommitteeHash
  ( UpdateCommitteeHashParams
      { sidechainParams
      , newCommitteePubKeys
      , committeeSignatures
      , previousMerkleRoot
      , sidechainEpoch
      }
  ) = do
  -- Getting the minting policy / currency symbol / token name for update
  -- committee hash
  -------------------------------------------------------------
  { committeeHashCurrencySymbol
  , committeeHashTokenName
  } ← getCommitteeHashPolicy sidechainParams

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams: sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    liftContractM
      (show (InternalError (InvalidScript "MerkleRootTokenCurrencySymbol")))
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Building the new committee hash / verifying that the new committee was
  -- signed (doing this offchain makes error messages better)...
  -------------------------------------------------------------
  when (null committeeSignatures)
    (throwContractError $ InvalidInputError "Empty Committee")

  let
    newCommitteeHash = Utils.Crypto.aggregateKeys newCommitteePubKeys

    curCommitteePubKeys /\ allCurCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures committeeSignatures
    _ /\ curCommitteeSignatures = Utils.Crypto.takeExactlyEnoughSignatures
      (unwrap sidechainParams).thresholdNumerator
      (unwrap sidechainParams).thresholdDenominator
      (curCommitteePubKeys /\ allCurCommitteeSignatures)
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys

  uchmsg ←
    liftContractM
      ( show
          ( InternalError
              (ConversionError "Failed to get update committee hash message")
          )
      )
      $ serialiseUchmHash
      $ UpdateCommitteeHashMessage
          { sidechainParams: sidechainParams
          , newCommitteePubKeys: newCommitteePubKeys
          , previousMerkleRoot: previousMerkleRoot
          , sidechainEpoch: sidechainEpoch
          }

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap sidechainParams).thresholdNumerator)
        ((unwrap sidechainParams).thresholdDenominator)
        curCommitteePubKeys
        uchmsg
        curCommitteeSignatures
    )
    ( throwContractError $
        InvalidInputError
          "Invalid committee signatures for UpdateCommitteeHashMessage"

    )

  -- Getting the validator / building the validator hash
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: assetClass committeeHashCurrencySymbol
          committeeHashTokenName
      , merkleRootTokenCurrencySymbol
      }
  updateValidator ← updateCommitteeHashValidator uch
  let valHash = Scripts.validatorHash updateValidator

  -- Grabbing the old committee / verifying that it really is the old committee
  -------------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo uch
  { index: oref
  , value:
      committeeHashTxOut@
        (TransactionOutputWithRefScript { output: TransactionOutput tOut })
  } ←
    liftContractM
      ( show
          ( InternalError
              (NotFoundUtxo "Failed to find update committee hash UTxO")
          )
      ) $ lkup

  rawDatum ←
    liftContractM
      ( show
          $ InternalError
          $ NotFoundUtxo "Committee hash UTxO is missing inline datum"
      )
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← liftContractM
    ( show
        $ InternalError
        $ ConversionError "Decoding datum at committee hash UTxO failed"

    )
    (fromData $ unwrap rawDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError (InvalidInputError "Incorrect committee provided"))

  -- Grabbing the last merkle root reference
  -------------------------------------------------------------
  maybePreviousMerkleRoot ← MerkleRoot.Utils.findPreviousMerkleRootTokenUtxo
    previousMerkleRoot
    smrm

  -- Building / submitting the transaction.
  -------------------------------------------------------------
  let
    newDatum = Datum $ toData
      ( UpdateCommitteeDatum
          { committeeHash: newCommitteeHash, sidechainEpoch }
      )
    value = assetClassValue (unwrap uch).uchAssetClass one
    redeemer = Redeemer $ toData
      ( UpdateCommitteeHashRedeemer
          { committeeSignatures: curCommitteeSignatures
          , committeePubKeys: curCommitteePubKeys
          , newCommitteePubKeys
          , previousMerkleRoot
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        (Map.singleton oref committeeHashTxOut)
        <> Lookups.validator updateValidator
        <> case maybePreviousMerkleRoot of
          Nothing → mempty
          Just { index: txORef, value: txOut } → Lookups.unspentOutputs
            (Map.singleton txORef txOut)
    constraints = TxConstraints.mustSpendScriptOutput oref redeemer
      <> TxConstraints.mustPayToScript valHash newDatum DatumInline value
      <> case maybePreviousMerkleRoot of
        Nothing → mempty
        Just { index: previousMerkleRootORef } → TxConstraints.mustReferenceOutput
          previousMerkleRootORef
  balanceSignAndSubmit "Update CommiteeHash" lookups constraints

-- | `findUpdateCommitteeHashUtxoFromSidechainParams` is similar to
-- | `findUpdateCommitteeHashUtxo` (and is indeed a small wrapper over it), but
-- | does the tricky work of grabbing the required currency symbols for you.
findUpdateCommitteeHashUtxoFromSidechainParams ∷
  SidechainParams →
  Contract { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript }
findUpdateCommitteeHashUtxoFromSidechainParams sidechainParams = do
  -- Getting the minting policy / currency symbol / token name for update
  -- committee hash
  -------------------------------------------------------------
  { committeeHashCurrencySymbol
  , committeeHashTokenName
  } ← getCommitteeHashPolicy sidechainParams

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams: sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    liftContractM
      ( show $ InternalError $ InvalidScript
          "Failed to get merkleRootTokenCurrencySymbol"
      )
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Build the UpdateCommitteeHash parameter
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: assetClass committeeHashCurrencySymbol
          committeeHashTokenName
      , merkleRootTokenCurrencySymbol
      }

  -- Finding the current committee
  -------------------------------------------------------------
  lkup ←
    liftedM (show $ InternalError $ NotFoundUtxo "current committee not found") $
      findUpdateCommitteeHashUtxo uch
  pure lkup

-- | `getCommitteeHashPolicy` grabs the committee hash policy, currency symbol and token name
-- | (potentially throwing an error in the case that it is not possible).
getCommitteeHashPolicy ∷
  SidechainParams →
  Contract
    { committeeHashPolicy ∷ MintingPolicy
    , committeeHashCurrencySymbol ∷ CurrencySymbol
    , committeeHashTokenName ∷ TokenName
    }
getCommitteeHashPolicy (SidechainParams sp) = do
  committeeHashPolicy ← committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: sp.genesisUtxo }
  committeeHashCurrencySymbol ← liftContractM
    (show (InternalError (InvalidScript "CommitteeHashPolicy")))
    (Value.scriptCurrencySymbol committeeHashPolicy)
  let committeeHashTokenName = initCommitteeHashMintTn
  pure
    { committeeHashPolicy, committeeHashCurrencySymbol, committeeHashTokenName }
