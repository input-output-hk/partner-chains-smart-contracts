module TrustlessSidechain.UpdateCommitteeHash
  ( module ExportTypes
  , module ExportUtils
  , updateCommitteeHash
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  ) where

import Contract.Prelude

import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.Value as Value
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , RedeemerDatum(RedeemerDatum)
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Data.Map as Map
import JS.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSAggregateSignatures
  , CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
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
import Type.Row (type (+))

-- | `UpdateCommitteeHashParams` is the offchain parameter for the update
-- | committee hash endpoint.
newtype UpdateCommitteeHashParams newAggregatePubKeys =
  UpdateCommitteeHashParams
    { sidechainParams ∷ SidechainParams
    , newAggregatePubKeys ∷ newAggregatePubKeys
    , aggregateSignature ∷ ATMSAggregateSignatures
    , previousMerkleRoot ∷ Maybe RootHash
    , sidechainEpoch ∷ BigInt -- sidechain epoch of the new committee
    , mNewCommitteeValidatorHash ∷ Maybe ScriptHash
    -- the address of the new committee (if it isn't provided, it will
    -- reuse the current committee address)
    }

derive instance Newtype (UpdateCommitteeHashParams newAggregatePubKeys) _

-- | `updateCommitteeHash` is the endpoint to submit the transaction to update
-- | the committee hash.
updateCommitteeHash ∷
  ∀ newAggregatePubKeys r.
  ToData newAggregatePubKeys ⇒
  UpdateCommitteeHashParams newAggregatePubKeys →
  Run (APP + r) TransactionHash
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
    Run.note
      ( ConversionError
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
        , message: Utils.Crypto.ecdsaSecp256k1MessageToAssetName scMsg
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
  ∀ newAggregatePubKeys r.
  ToData newAggregatePubKeys ⇒
  { sidechainParams ∷ SidechainParams
  , previousMerkleRoot ∷ Maybe RootHash
  , sidechainEpoch ∷ BigInt
  , newAggregatePubKeys ∷ newAggregatePubKeys
  , committeeCertificateVerificationCurrencySymbol ∷ ScriptHash
  , mNewCommitteeValidatorHash ∷ Maybe ScriptHash
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookupsAndConstraints ∷
        { constraints ∷ TxConstraints
        , lookups ∷ ScriptLookups
        }
    , currentCommitteeUtxo ∷
        { index ∷ TransactionInput
        , value ∷ TransactionOutput
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
    Run.note
      (NotFoundUtxo "Failed to find committee UTxO") $ lkup

  -- Grabbing the last merkle root reference
  -------------------------------------------------------------
  maybePreviousMerkleRoot ←
    findPreviousMerkleRootTokenUtxo previousMerkleRoot sidechainParams

  committeeOracleCurrencySymbol ←
    Versioning.getVersionedCurrencySymbol
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: CommitteeOraclePolicy }
      )

  (mrtPolicyRefTxInput /\ mrtPolicyRefTxOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: MerkleRootTokenPolicy }
      )

  -- Building the transaction.
  -------------------------------------------------------------
  let
    newDatum = toData
      ( UpdateCommitteeDatum
          { aggregatePubKeys: toData newAggregatePubKeys, sidechainEpoch }
      )
    value =
      Value.singleton
        committeeOracleCurrencySymbol
        CommitteeOraclePolicy.committeeOracleTn
        (BigNum.fromInt 1)
    uchm = UpdateCommitteeHashMessage
      { sidechainParams
      , newAggregatePubKeys: toData newAggregatePubKeys
      , previousMerkleRoot
      , sidechainEpoch
      , validatorHash: newValidatorHash
      }

    redeemer = RedeemerDatum $ toData $ UpdateCommitteeHashRedeemer
      { previousMerkleRoot }

    lookups ∷ Lookups.ScriptLookups
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
