module TrustlessSidechain.UpdateCommitteeHash
  ( module ExportTypes
  , module ExportUtils
  , updateCommitteeHash
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  ) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , liftContractM
  )
import Contract.PlutusData
  ( class ToData
  , Datum(Datum)
  , PlutusData
  , Redeemer(Redeemer)
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.Error
  ( OffchainError(ConversionError, NotFoundUtxo)
  )
import TrustlessSidechain.MerkleRoot.Utils
  ( findPreviousMerkleRootTokenUtxo
  )
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  )
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
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
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.Types
  ( ScriptId(CommitteeOraclePolicy, MerkleRootTokenPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `UpdateCommitteeHashParams` is the offchain parameter for the update
-- | committee hash endpoint.
newtype UpdateCommitteeHashParams newAggregatePubKeys =
  UpdateCommitteeHashParams
    { sidechainParams ∷ SidechainParams
    , newAggregatePubKeys ∷ newAggregatePubKeys
    , aggregateSignature ∷ ATMSAggregateSignatures
    , previousMerkleRoot ∷ Maybe RootHash
    , sidechainEpoch ∷ BigInt -- sidechain epoch of the new committee
    , mNewCommitteeValidatorHash ∷ Maybe ValidatorHash
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
      , mNewCommitteeValidatorHash
      }
  ) = do
  -- Set up for the committee ATMS schemes
  ------------------------------------
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator:
            (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator:
            (unwrap sidechainParams).thresholdDenominator
        }
  { currencySymbol: committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      { committeeCertificateMint, sidechainParams }
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
    , mNewCommitteeValidatorHash
    }

  -- Committee ATMS scheme lookups and constraints
  ------------------------------------

  scMsg ←
    liftContractM
      ( show $ ConversionError
          "bad UpdateCommitteeHashMessage serialization"
      )
      $ serialiseUchmHash
      $
        updateCommitteeHashMessage
  committeeATMSLookupsAndConstraints ←
    CommitteeATMSSchemes.atmsSchemeLookupsAndConstraints sidechainParams $
      CommitteeATMSParams
        { currentCommitteeUtxo
        , committeeCertificateMint
        , aggregateSignature
        , message: Utils.Crypto.ecdsaSecp256k1MessageToTokenName scMsg
        }

  balanceSignAndSubmit "Update CommiteeHash"
    ( lookupsAndConstraints
        <> committeeATMSLookupsAndConstraints
    )

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
  , mNewCommitteeValidatorHash ∷ Maybe ValidatorHash
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
  , mNewCommitteeValidatorHash
  } = do

  -- Getting the validator / building the validator hash
  -------------------------------------------------------------
  { validator: updateValidator
  , validatorHash: updateValidatorHash
  } ← getUpdateCommitteeHashValidator sidechainParams

  -- if we have provided a new validator hash to upgrade to, then move
  -- the NFT to an address coressponding to the new validator hash
  -- otherwise assume that we are paying back to the original validator address.
  let newValidatorHash = fromMaybe updateValidatorHash mNewCommitteeValidatorHash

  -- Get the UTxO with the current committee
  ------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo sidechainParams
  currentCommitteeUtxo@
    { index: oref
    , value:
        committeeOracleTxOut
    } ←
    liftContractM
      (show $ NotFoundUtxo "Failed to find committee UTxO") $ lkup

  -- Grabbing the last merkle root reference
  -------------------------------------------------------------
  maybePreviousMerkleRoot ←
    findPreviousMerkleRootTokenUtxo previousMerkleRoot sidechainParams

  committeeOracleCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: CommitteeOraclePolicy }
      )

  (mrtPolicyRefTxInput /\ mrtPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: MerkleRootTokenPolicy }
      )

  -- Building the transaction.
  -------------------------------------------------------------
  let
    newDatum = Datum $ toData
      ( UpdateCommitteeDatum
          { aggregatePubKeys: toData newAggregatePubKeys, sidechainEpoch }
      )
    value =
      Value.singleton
        committeeOracleCurrencySymbol
        CommitteeOraclePolicy.committeeOracleTn
        one
    uchm = UpdateCommitteeHashMessage
      { sidechainParams
      , newAggregatePubKeys: toData newAggregatePubKeys
      , previousMerkleRoot
      , sidechainEpoch
      , validatorHash: newValidatorHash
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
        <> Lookups.unspentOutputs
          (Map.singleton mrtPolicyRefTxInput mrtPolicyRefTxOutput)
    constraints = TxConstraints.mustSpendScriptOutput oref redeemer
      <> TxConstraints.mustPayToScript newValidatorHash newDatum DatumInline value
      <> case maybePreviousMerkleRoot of
        Nothing → mempty
        Just { index: previousMerkleRootORef } → TxConstraints.mustReferenceOutput
          previousMerkleRootORef
      <> TxConstraints.mustReferenceOutput mrtPolicyRefTxInput

  pure
    { lookupsAndConstraints: { lookups, constraints }
    , currentCommitteeUtxo
    , updateCommitteeHashMessage: uchm
    }
