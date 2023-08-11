module TrustlessSidechain.UpdateCommitteeHash
  ( module ExportTypes
  , module ExportUtils
  , updateCommitteeHash
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Address as Address
import Contract.Monad
  ( Contract
  , liftContractM
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
  )
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
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
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  )
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
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
import TrustlessSidechain.Utils.Logging
  ( InternalError(ConversionError, InvalidScript, NotFoundUtxo)
  , OffchainError(InternalError)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

-- | `UpdateCommitteeHashParams` is the offchain parameter for the update
-- | committee hash endpoint.
newtype UpdateCommitteeHashParams newAggregatePubKeys =
  UpdateCommitteeHashParams
    { sidechainParams ∷ SidechainParams
    , newAggregatePubKeys ∷ newAggregatePubKeys
    , aggregateSignature ∷ ATMSAggregateSignatures
    , previousMerkleRoot ∷ Maybe RootHash
    , sidechainEpoch ∷ BigInt -- sidechain epoch of the new committee
    , mNewCommitteeAddress ∷ Maybe Address
    -- the address of the new committee (if it isn't provided, it will
    -- reuse the current committee address)
    }

derive instance Newtype (UpdateCommitteeHashParams newAggregatePubKeys) _

-- | `updateCommitteeHash` is the endpoint to submit the transaction to update
-- | the committee hash.
updateCommitteeHash ∷
  ∀ newAggregatePubKeys.
  ToData newAggregatePubKeys ⇒
  UpdateCommitteeHashParams newAggregatePubKeys →
  Contract TransactionHash
updateCommitteeHash
  ( UpdateCommitteeHashParams
      { sidechainParams
      , previousMerkleRoot
      , sidechainEpoch
      , newAggregatePubKeys
      , aggregateSignature
      , mNewCommitteeAddress
      }
  ) = do
  -- Set up for the committee ATMS schemes
  ------------------------------------
  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sidechainParams
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator:
            (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator:
            (unwrap sidechainParams).thresholdDenominator
        , committeeOraclePolicy: committeeOracleCurrencySymbol
        }
  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      committeeCertificateMint
      aggregateSignature

  -- Update comittee lookups and constraints
  ------------------------------------
  { lookupsAndConstraints
  , currentCommitteeUtxo
  , updateCommitteeHashMessage
  } ← updateCommitteeHashLookupsAndConstraints
    { sidechainParams
    , previousMerkleRoot
    , sidechainEpoch
    , newAggregatePubKeys
    , committeeCertificateVerificationCurrencySymbol
    , mNewCommitteeAddress
    }

  -- Committee ATMS scheme lookups and constraints
  ------------------------------------

  scMsg ←
    liftContractM
      ( show $ InternalError $ ConversionError
          "bad UpdateCommitteeHashMessage serialization"
      )
      $ serialiseUchmHash
      $
        updateCommitteeHashMessage
  committeeATMSLookupsAndConstraints ←
    CommitteeATMSSchemes.atmsSchemeLookupsAndConstraints $ CommitteeATMSParams
      { currentCommitteeUtxo
      , committeeCertificateMint
      , aggregateSignature
      , message: Utils.Crypto.ecdsaSecp256k1MessageToTokenName scMsg
      }

  let
    { lookups, constraints } = lookupsAndConstraints
      <> committeeATMSLookupsAndConstraints

  balanceSignAndSubmit "Update CommiteeHash" lookups constraints

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
  , mNewCommitteeAddress ∷ Maybe Address
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
  , mNewCommitteeAddress
  } = do
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
      , committeeCertificateVerificationCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    liftContractM
      (show (InternalError (InvalidScript "MerkleRootTokenCurrencySymbol")))
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
  , validatorHash: _valHash
  , address: updateValidatorAddress
  } ← getUpdateCommitteeHashValidator uch

  -- if we have provided a new validator address to upgrade to, then move
  -- the NFT to the new validator address -- otherwise; assume that we are paying
  -- back to the original validator address.
  { newValidatorHash, newValidatorAddress } ← do
    let
      newValidatorAddress = case mNewCommitteeAddress of
        Nothing → updateValidatorAddress
        Just x → x
    newValidatorHash ←
      liftContractM
        ( show
            $ InternalError
            $ InvalidScript
                "Failed to get validator hash from provided new address"
        )
        $ Address.addressPaymentValidatorHash newValidatorAddress
    pure
      { newValidatorAddress
      , newValidatorHash
      }

  -- Get the UTxO with the current committee
  ------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo uch
  currentCommitteeUtxo@
    { index: oref
    , value:
        committeeOracleTxOut
    } ←
    liftContractM
      (show $ InternalError $ NotFoundUtxo "Failed to find committee UTxO") $ lkup

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
      , validatorAddress: newValidatorAddress
      }

    redeemer = Redeemer $ toData $ UpdateCommitteeHashRedeemer
      { previousMerkleRoot }

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (Map.singleton oref committeeOracleTxOut)
        <> Lookups.validator updateValidator
        <> case maybePreviousMerkleRoot of
          Nothing → mempty
          Just { index: txORef, value: txOut } → Lookups.unspentOutputs
            (Map.singleton txORef txOut)
    constraints = TxConstraints.mustSpendScriptOutput oref redeemer
      <> TxConstraints.mustPayToScript newValidatorHash newDatum DatumInline value
      <> case maybePreviousMerkleRoot of
        Nothing → mempty
        Just { index: previousMerkleRootORef } → TxConstraints.mustReferenceOutput
          previousMerkleRootORef

  pure
    { lookupsAndConstraints: { lookups, constraints }
    , currentCommitteeUtxo
    , updateCommitteeHashMessage: uchm
    }
