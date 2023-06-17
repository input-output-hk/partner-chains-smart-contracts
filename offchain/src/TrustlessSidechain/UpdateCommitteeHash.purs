module TrustlessSidechain.UpdateCommitteeHash
  ( module ExportTypes
  , module ExportUtils
  , updateCommitteeHash
  , getCommitteeHashPolicy
  , findUpdateCommitteeHashUtxoFromSidechainParams
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
import Contract.PlutusData
  ( class ToData
  , Datum(..)
  , PlutusData
  , Redeemer(..)
  , fromData
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutput(..)
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceTx
  , outputDatumDatum
  , signTransaction
  , submit
  )
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.Map as Map
import TrustlessSidechain.CommitteeOraclePolicy
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy
  ( committeeHashPolicy
  , initCommitteeHashMintTn
  )
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (assetClass, assetClassValue)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  ) as ExportTypes
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( findUpdateCommitteeHashUtxo
  , serialiseUchmHash
  , updateCommitteeHashValidator
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( findUpdateCommitteeHashUtxo
  , serialiseUchmHash
  , updateCommitteeHashValidator
  ) as ExportUtils
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging as Logging

-- | `updateCommitteeHash` is the endpoint to submit the transaction to update
-- | the committee hash.
updateCommitteeHash ∷
  UpdateCommitteeHashParams (Array SidechainPublicKey) → Contract TransactionHash
updateCommitteeHash params = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "findUpdateCommitteeHashUtxoFromSidechainParams"
  -- runUpdateCommitteeHash params

  { lookupsAndConstraints: updateCommitteeHashLookupsAndConstraints
  , currentCommitteeUtxo
  } ← updateCommitteeHashLookupsAndConstraints params
  -- TODO pick up from here

  let { lookups, constraints } = updateCommitteeHashLookupsAndConstraints

  ubTx ← liftedE
    (lmap (show >>> mkErr) <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap (show >>> mkErr) <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (mkErr "Submitted update committee hash transaction: " <> show txId)
  awaitTxConfirmed txId
  logInfo' (mkErr "Update committee hash transaction submitted successfully")

  pure txId

-- | `updateCommitteeHashLookupsAndConstraints` grabs the lookups and
-- | constraints for updating the committee hash, and returns the current
-- | committee UTxO.
-- | In particular, it creates lookups for:
-- |    - the current committee UTxO
-- |    - the update committee hash validator
-- |    - the UTxO with the previous merkle root (if it exists)
-- | and creates constraints for:
-- |    - spending the current committee UTxO
-- |    - paying the committee oracle NFT (committee hash policy) to the same
-- |    committee hash validator
-- |    - referencing the merkle root UTxO (if it exists)
updateCommitteeHashLookupsAndConstraints ∷
  ∀ newAggregatePubKeys.
  ToData newAggregatePubKeys ⇒
  UpdateCommitteeHashParams newAggregatePubKeys →
  Contract
    { lookupsAndConstraints ∷
        { constraints ∷ TxConstraints Void Void
        , lookups ∷ ScriptLookups Void
        }
    , currentCommitteeUtxo ∷
        { index ∷ TransactionInput
        , value ∷ TransactionOutputWithRefScript
        }
    }
updateCommitteeHashLookupsAndConstraints
  ( UpdateCommitteeHashParams
      { sidechainParams
      , newCommitteePubKeys
      , committeeSignatures
      , previousMerkleRoot
      , sidechainEpoch
      }
  ) = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "runUpdateCommitteeHash"

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
      (mkErr "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Getting the validator / building the validator hash
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootTokenCurrencySymbol
      }
  updateValidator ← updateCommitteeHashValidator uch
  let valHash = Scripts.validatorHash updateValidator

  -- Get the UTxO with the current committee
  ------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo uch
  currentCommitteeUtxo@
    { index: oref
    , value:
        committeeHashTxOut@
          (TransactionOutputWithRefScript { output: TransactionOutput tOut })
    } ←
    liftContractM (mkErr "Failed to find committee UTxO") $ lkup

  -- Grabbing the last merkle root reference
  -------------------------------------------------------------
  maybePreviousMerkleRoot ← MerkleRoot.Utils.findPreviousMerkleRootTokenUtxo
    previousMerkleRoot
    smrm

  -- Building the transaction.
  -------------------------------------------------------------
  let
    newDatum = Datum $ toData
      ( UpdateCommitteeDatum
          { committeeHash: toData newCommitteePubKeys, sidechainEpoch }
      )
    value =
      Value.singleton
        (unwrap uch).committeeOracleCurrencySymbol
        initCommitteeHashMintTn
        one
    redeemer = Redeemer $ toData
      ( UpdateCommitteeHashMessage
          { sidechainParams
          , newCommitteePubKeys: toData newCommitteePubKeys
          , previousMerkleRoot
          , sidechainEpoch
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (Map.singleton oref committeeHashTxOut)
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

  pure
    { lookupsAndConstraints: { lookups, constraints }
    , currentCommitteeUtxo
    }

-- | `runUpdateCommitteeHash` is the main worker function of the
-- | `updateCommitteeHash` endpoint.
-- | Preconditions:
-- |    - `UpdateCommitteeHashParams` has been normalized via
-- |    `UpdateCommitteeHash.Utils.normalizeCommitteeHashParams`
runUpdateCommitteeHash ∷
  UpdateCommitteeHashParams (Array SidechainPublicKey) → Contract TransactionHash
runUpdateCommitteeHash
  ( UpdateCommitteeHashParams
      { sidechainParams
      , newCommitteePubKeys
      , committeeSignatures
      , previousMerkleRoot
      , sidechainEpoch
      }
  ) = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "runUpdateCommitteeHash"

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
      (mkErr "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Building the new committee hash / verifying that the new committee was
  -- signed (doing this offchain makes error messages better)...
  -------------------------------------------------------------
  when (null committeeSignatures)
    (throwContractError $ mkErr "Empty Committee")

  let
    newCommitteeHash = Utils.Crypto.aggregateKeys newCommitteePubKeys

    curCommitteePubKeys /\ allCurCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures committeeSignatures
    _ /\ curCommitteeSignatures = Utils.Crypto.takeExactlyEnoughSignatures
      (unwrap sidechainParams).thresholdNumerator
      (unwrap sidechainParams).thresholdDenominator
      (curCommitteePubKeys /\ allCurCommitteeSignatures)
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys

  uchmsg ← liftContractM (mkErr "Failed to get update committee hash message")
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
    ( throwContractError $ mkErr
        "Invalid committee signatures for UpdateCommitteeHashMessage"
    )

  -- Getting the validator / building the validator hash
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: committeeHashCurrencySymbol
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
    liftContractM (mkErr "Failed to find update committee hash UTxO") $ lkup

  rawDatum ←
    liftContractM (mkErr "Update committee hash UTxO is missing inline datum")
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← liftContractM
    (mkErr "Datum at update committee hash UTxO fromData failed")
    (fromData $ unwrap rawDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError "Incorrect committee provided")

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
    value = Value.singleton (unwrap uch).committeeOracleCurrencySymbol
      initCommitteeHashMintTn
      one
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

  ubTx ← liftedE
    (lmap (show >>> mkErr) <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap (show >>> mkErr) <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (mkErr "Submitted update committee hash transaction: " <> show txId)
  awaitTxConfirmed txId
  logInfo' (mkErr "Update committee hash transaction submitted successfully")

  pure txId

-- | `findUpdateCommitteeHashUtxoFromSidechainParams` is similar to
-- | `findUpdateCommitteeHashUtxo` (and is indeed a small wrapper over it), but
-- | does the tricky work of grabbing the required currency symbols for you.
findUpdateCommitteeHashUtxoFromSidechainParams ∷
  SidechainParams →
  Contract { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript }
findUpdateCommitteeHashUtxoFromSidechainParams sidechainParams = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "findUpdateCommitteeHashUtxoFromSidechainParams"

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
      (mkErr "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Build the UpdateCommitteeHash parameter
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootTokenCurrencySymbol
      }

  -- Finding the current committee
  -------------------------------------------------------------
  lkup ← liftedM (mkErr "current committee not found") $
    findUpdateCommitteeHashUtxo uch
  pure lkup

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → String → String
report = Logging.mkReport "UpdateCommitteeHash"

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
  let
    mkErr = report "getCommitteeHashPolicy"
  committeeHashPolicy ← committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: sp.genesisUtxo }
  committeeHashCurrencySymbol ← liftContractM
    (mkErr "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)
  let committeeHashTokenName = initCommitteeHashMintTn
  pure
    { committeeHashPolicy, committeeHashCurrencySymbol, committeeHashTokenName }
