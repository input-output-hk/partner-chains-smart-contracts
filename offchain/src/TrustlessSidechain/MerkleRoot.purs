-- | `MerkleRoot` contains the endpoint functionality for the `MerkleRoot` endpoint
module TrustlessSidechain.MerkleRoot
  ( module ExportTypes
  , module ExportUtils
  , saveRoot
  ) where

import Contract.Prelude hiding (unit)

import Contract.PlutusData
  ( toData
  )
import Cardano.Types.PlutusData (unit)
import Contract.Numeric.BigNum as BigNum
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  )
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput))
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Cardano.Types.AssetName (mkAssetName)
import Contract.TxConstraints as TxConstraints
import Cardano.Types.Value as Value
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.CommitteeATMSSchemes
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Util as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(InvalidData, NotFoundUtxo)
  )
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  )
import TrustlessSidechain.MerkleRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRootRedeemer(SignedMerkleRootRedeemer)
  ) as ExportTypes
import TrustlessSidechain.MerkleRoot.Utils
  ( findMerkleRootTokenUtxo
  , merkleRootCurrencyInfo
  , merkleRootTokenValidator
  , serialiseMrimHash
  ) as ExportUtils
import TrustlessSidechain.MerkleRoot.Utils
  ( findPreviousMerkleRootTokenUtxo
  , merkleRootCurrencyInfo
  , merkleRootTokenValidator
  , serialiseMrimHash
  )
import Cardano.Types.PlutusScript as PlutusScript
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( MerkleRootTokenPolicy
      , MerkleRootTokenValidator
      , CommitteeCertificateVerificationPolicy
      )
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))
import Cardano.Types.Int as Int

-- | `saveRoot` is the endpoint.
saveRoot ∷
  ∀ r.
  SaveRootParams →
  Run (APP + r) TransactionHash
saveRoot
  ( SaveRootParams
      { sidechainParams
      , merkleRoot
      , previousMerkleRoot
      , aggregateSignature
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

  -- Find the UTxO with the current committee.
  ------------------------------------
  currentCommitteeUtxo ←
    Effect.fromMaybeThrow
      ( NotFoundUtxo "failed to find current committee UTxO"
      )
      $ UpdateCommitteeHash.findUpdateCommitteeHashUtxo
          sidechainParams

  -- Grab the lookups + constraints for saving a merkle root
  ------------------------------------
  { lookupsAndConstraints
  , merkleRootInsertionMessage
  } ← saveRootLookupsAndConstraints
    { sidechainParams
    , merkleRoot
    , previousMerkleRoot
    }

  -- Grab the lookups + constraints for the committee certificate
  -- verification
  ------------------------------------
  scMsg ←
    Run.note
      (InvalidData "failed serializing the MerkleRootInsertionMessage")
      $
        serialiseMrimHash merkleRootInsertionMessage

  atmsLookupsAndConstraints ←
    CommitteeATMSSchemes.atmsSchemeLookupsAndConstraints sidechainParams
      $ CommitteeATMSParams
          { currentCommitteeUtxo
          , committeeCertificateMint
          , aggregateSignature
          , message: Utils.Crypto.ecdsaSecp256k1MessageToAssetName scMsg
          }

  -- Building the transaction / submitting it
  ------------------------------------
  let
    { lookups, constraints } =
      lookupsAndConstraints
        <> atmsLookupsAndConstraints
        <>
          -- include the current committee UTxO  in this transaction
          { lookups:
              Lookups.unspentOutputs
                ( Map.singleton currentCommitteeUtxo.index
                    currentCommitteeUtxo.value
                )
          , constraints:
              TxConstraints.mustReferenceOutput currentCommitteeUtxo.index
          }

  balanceSignAndSubmit "Save Merkle root" { lookups, constraints }

-- | `saveRootLookupsAndConstraints` creates the lookups and constraints (and
-- | the message to be signed) for saving a Merkle root
saveRootLookupsAndConstraints ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ RootHash
  , previousMerkleRoot ∷ Maybe RootHash
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookupsAndConstraints ∷
        { constraints ∷ TxConstraints
        , lookups ∷ ScriptLookups
        }
    , merkleRootInsertionMessage ∷ MerkleRootInsertionMessage
    }
saveRootLookupsAndConstraints
  { sidechainParams
  , merkleRoot
  , previousMerkleRoot
  } = do

  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  { mintingPolicy: rootTokenMP
  , currencySymbol: rootTokenCS
  } ← merkleRootCurrencyInfo sidechainParams
  rootTokenVal ← merkleRootTokenValidator sidechainParams
  merkleRootTokenName ←
    Run.note
      ( InvalidData
          "Invalid Merkle root TokenName for merkleRootTokenMintingPolicy"

      )
      $ mkAssetName
      $ MerkleTree.unRootHash merkleRoot

  -- Grab the transaction holding the last merkle root
  ---------------------------------------------------------
  maybePreviousMerkleRootUtxo ← findPreviousMerkleRootTokenUtxo
    previousMerkleRoot
    sidechainParams

  -- Building the transaction
  ---------------------------------------------------------

  ( committeeCertificateVerificationVersioningInput /\
      committeeCertificateVerificationVersioningOutput
  ) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigNum.fromInt 1
        , scriptId: CommitteeCertificateVerificationPolicy
        }
    )

  (merkleRootValidatorVersioningInput /\ merkleRootValidatorVersioningOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: MerkleRootTokenValidator }
      )

  (merkleRootPolicyVersioningInput /\ merkleRootPolicyVersioningOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: MerkleRootTokenPolicy }
      )

  let
    value = Value.singleton rootTokenCS merkleRootTokenName (BigNum.fromInt 1)

    msg = MerkleRootInsertionMessage
      { sidechainParams
      , merkleRoot
      , previousMerkleRoot
      }

    redeemer = ExportTypes.SignedMerkleRootRedeemer
      { previousMerkleRoot
      }

    constraints ∷ TxConstraints
    constraints =
      TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
        (PlutusScript.hash rootTokenMP)
        (wrap (toData redeemer))
        merkleRootTokenName
        (Int.fromInt 1)
        ( RefInput $ TransactionUnspentOutput
              { input: merkleRootPolicyVersioningInput
              , output: merkleRootPolicyVersioningOutput
              }
        )
        <> TxConstraints.mustPayToScript (PlutusScript.hash rootTokenVal)
          unit
          TxConstraints.DatumWitness
          value
        <> TxConstraints.mustReferenceOutput merkleRootValidatorVersioningInput
        <> TxConstraints.mustReferenceOutput
          committeeCertificateVerificationVersioningInput
        <> case maybePreviousMerkleRootUtxo of
          Nothing → mempty
          Just { index: oref } → TxConstraints.mustReferenceOutput oref

    lookups ∷ Lookups.ScriptLookups
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton merkleRootValidatorVersioningInput
            merkleRootValidatorVersioningOutput
        )
        <> Lookups.unspentOutputs
          ( Map.singleton committeeCertificateVerificationVersioningInput
              committeeCertificateVerificationVersioningOutput
          )
        <> Lookups.unspentOutputs
          ( Map.singleton merkleRootPolicyVersioningInput
              merkleRootPolicyVersioningOutput
          )
        <> case maybePreviousMerkleRootUtxo of
          Nothing → mempty
          Just { index: txORef, value: txOut } → Lookups.unspentOutputs
            (Map.singleton txORef txOut)

  pure
    { lookupsAndConstraints: { lookups, constraints }
    , merkleRootInsertionMessage: msg
    }
