module TrustlessSidechain.UpdateCommitteeHash
  ( module ExportTypes
  , module ExportUtils
  , updateCommitteeHash
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  )
import Contract.PlutusData
  ( class ToData
  , Datum(..)
  , PlutusData
  , Redeemer(..)
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  )
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  ) as ExportTypes
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( findUpdateCommitteeHashUtxo
  , getUpdateCommitteeHashValidator
  , serialiseUchmHash
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( findUpdateCommitteeHashUtxo
  , getUpdateCommitteeHashValidator
  , serialiseUchmHash
  , updateCommitteeHashValidator
  ) as ExportUtils
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging as Logging

-- | `UpdateCommitteeHashParams` is the offchain parameter for the update
-- | committee hash endpoint.
newtype UpdateCommitteeHashParams newAggregatePubKeys =
  UpdateCommitteeHashParams
    { sidechainParams ∷ SidechainParams
    , newAggregatePubKeys ∷ newAggregatePubKeys
    , aggregateSignature ∷ ATMSAggregateSignatures
    , previousMerkleRoot ∷ Maybe RootHash
    , sidechainEpoch ∷ BigInt -- sidechain epoch of the new committee
    }

derive instance Newtype (UpdateCommitteeHashParams newAggregatePubKeys) _

-- | `updateCommitteeHash` is the endpoint to submit the transaction to update
-- | the committee hash.
updateCommitteeHash ∷
  ∀ newAggregatePubKeys.
  ToData newAggregatePubKeys ⇒
  UpdateCommitteeHashParams newAggregatePubKeys →
  Contract TransactionHash
updateCommitteeHash params = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "updateCommitteeHash"

  -- Set up for the committee ATMS schemes
  ------------------------------------
  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy
      (unwrap params).sidechainParams
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator:
            (unwrap ((unwrap params).sidechainParams)).thresholdNumerator
        , thresholdDenominator:
            (unwrap (unwrap params).sidechainParams).thresholdDenominator
        , committeeOraclePolicy: committeeOracleCurrencySymbol
        }
  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      committeeCertificateMint
      (unwrap params).aggregateSignature

  -- Update comittee lookups and constraints
  ------------------------------------
  { lookupsAndConstraints
  , currentCommitteeUtxo
  , updateCommitteeHashMessage
  } ← updateCommitteeHashLookupsAndConstraints
    { sidechainParams: (unwrap params).sidechainParams
    , previousMerkleRoot: (unwrap params).previousMerkleRoot
    , sidechainEpoch: (unwrap params).sidechainEpoch
    , newAggregatePubKeys: (unwrap params).newAggregatePubKeys
    , committeeCertificateVerificationCurrencySymbol
    }

  -- Committee ATMS scheme lookups and constraints
  ------------------------------------

  scMsg ← liftContractM (mkErr "bad UpdateCommitteeHashMessage serialization")
    $ serialiseUchmHash
    $
      updateCommitteeHashMessage
  committeeATMSLookupsAndConstraints ←
    CommitteeATMSSchemes.atmsSchemeLookupsAndConstraints $ CommitteeATMSParams
      { currentCommitteeUtxo
      , committeeCertificateMint
      , aggregateSignature: (unwrap params).aggregateSignature
      , message: Utils.Crypto.sidechainMessageToTokenName scMsg
      }

  let
    { lookups, constraints } = lookupsAndConstraints
      <> committeeATMSLookupsAndConstraints
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
  { sidechainParams ∷ SidechainParams
  , previousMerkleRoot ∷ Maybe RootHash
  , sidechainEpoch ∷ BigInt
  , newAggregatePubKeys ∷ newAggregatePubKeys
  , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
  } →
  Contract
    { lookupsAndConstraints ∷
        { constraints ∷ TxConstraints Void Void
        , lookups ∷ ScriptLookups Void
        }
    , currentCommitteeUtxo ∷
        { index ∷ TransactionInput
        , value ∷ TransactionOutputWithRefScript
        }
    , updateCommitteeHashMessage ∷ UpdateCommitteeHashMessage PlutusData
    }
updateCommitteeHashLookupsAndConstraints
  { sidechainParams
  , newAggregatePubKeys
  , previousMerkleRoot
  , sidechainEpoch
  , committeeCertificateVerificationCurrencySymbol
  } = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "updateCommitteeHashLookupsAndConstraints"

  -- Getting the minting policy / currency symbol / token name for update
  -- committee hash
  -------------------------------------------------------------
  { committeeOracleCurrencySymbol
  } ← CommitteeOraclePolicy.getCommitteeOraclePolicy sidechainParams

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams: sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeOracleCurrencySymbol
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
      , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol
      , merkleRootTokenCurrencySymbol
      }

  { validator: updateValidator
  , validatorHash: valHash
  , address: updateValidatorAddress
  } ← getUpdateCommitteeHashValidator uch

  -- Get the UTxO with the current committee
  ------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo uch
  currentCommitteeUtxo@
    { index: oref
    , value:
        committeeOracleTxOut
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
          { aggregatePubKeys: toData newAggregatePubKeys, sidechainEpoch }
      )
    value =
      Value.singleton
        (unwrap uch).committeeOracleCurrencySymbol
        CommitteeOraclePolicy.committeeOracleTn
        one
    uchm = UpdateCommitteeHashMessage
      { sidechainParams
      , newAggregatePubKeys: toData newAggregatePubKeys
      , previousMerkleRoot
      , sidechainEpoch
      , validatorAddress: updateValidatorAddress
      }
    redeemer = Redeemer $ toData uchm

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (Map.singleton oref committeeOracleTxOut)
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
    , updateCommitteeHashMessage: uchm
    }

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → String → String
report = Logging.mkReport "UpdateCommitteeHash"
