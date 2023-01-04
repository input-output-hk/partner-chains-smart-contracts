module UpdateCommitteeHash
  ( module UpdateCommitteeHash.Types
  , module UpdateCommitteeHash.Utils
  , updateCommitteeHash
  , getCommitteeHashPolicy
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, throwContractError)
import Contract.PlutusData (Datum(..), Redeemer(..), fromData, toData)
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
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import MPTRoot.Types (SignedMerkleRootMint(SignedMerkleRootMint))
import MPTRoot.Utils as MPTRoot.Utils
import SidechainParams (SidechainParams(..))
import SidechainParams as SidechainParams
import Types (assetClass, assetClassValue)
import UpdateCommitteeHash.Types
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  )
import UpdateCommitteeHash.Utils
  ( aggregateKeys
  , committeeHashAssetClass
  , committeeHashPolicy
  , findUpdateCommitteeHashUtxo
  , initCommitteeHashMintTn
  , normalizeCommitteeHashParams
  , serialiseUchmHash
  , updateCommitteeHashValidator
  )
import Utils.Crypto as Utils.Crypto
import Utils.Logging (class Display)
import Utils.Logging as Logging
import Utils.Logging as Utils.Logging

-- | `updateCommitteeHash` is the endpoint to submit the transaction to update
-- | the committee hash.
updateCommitteeHash ∷ UpdateCommitteeHashParams → Contract () TransactionHash
updateCommitteeHash = runUpdateCommitteeHash <<< normalizeCommitteeHashParams

-- | `runUpdateCommitteeHash` is the main worker function of the
-- | `updateCommitteeHash` endpoint.
-- | Preconditions:
-- |    - `UpdateCommitteeHashParams` has been normalized via
-- |    `UpdateCommitteeHash.Utils.normalizeCommitteeHashParams`
runUpdateCommitteeHash ∷ UpdateCommitteeHashParams → Contract () TransactionHash
runUpdateCommitteeHash
  ( UpdateCommitteeHashParams
      { sidechainParams
      , newCommitteePubKeys
      , committeeSignatures
      , previousMerkleRoot
      , sidechainEpoch
      }
  ) = do
  let -- `msg` is used to help generate log messages
    msg = report "runUpdateCommitteeHash"

  -- Getting the minting policy / currency symbol / token name for update
  -- committee hash
  -------------------------------------------------------------
  pol ← committeeHashPolicy
    ( InitCommitteeHashMint
        { icTxOutRef: (\(SidechainParams x) → x.genesisUtxo) sidechainParams
        }
    )

  cs ← liftContractM (msg "Failed to get updateCommitteeHash minting policy")
    $ Value.scriptCurrencySymbol pol

  let tn = initCommitteeHashMintTn

  -- Getting the minting policy for the mpt root token
  -------------------------------------------------------------
  let
    smrm = SignedMerkleRootMint
      { sidechainParams: sidechainParams
      , updateCommitteeHashCurrencySymbol: cs
      }
  mptRootTokenMintingPolicy ← MPTRoot.Utils.mptRootTokenMintingPolicy smrm
  mptRootTokenCurrencySymbol ←
    liftContractM
      (msg "Failed to get mptRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol mptRootTokenMintingPolicy

  -- Building the new committee hash / verifying that the new committee was
  -- signed (doing this offchain makes error messages better)...
  -------------------------------------------------------------
  when (null committeeSignatures)
    (throwContractError $ msg "Empty Committee")

  let
    newCommitteeHash = aggregateKeys newCommitteePubKeys

    curCommitteePubKeys /\ curCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures committeeSignatures
    curCommitteeHash = aggregateKeys curCommitteePubKeys

  uchmsg ← liftContractM (msg "Failed to get update committee hash message")
    $ serialiseUchmHash
    $ UpdateCommitteeHashMessage
        { sidechainParams: SidechainParams.convertSCParams sidechainParams
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
    ( throwContractError $ msg
        "Invalid committee signatures for UpdateCommitteeHashMessage"
    )

  -- Getting the validator / building the validator hash
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: assetClass cs tn
      , mptRootTokenCurrencySymbol
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
    liftContractM (msg "Failed to find update committee hash UTxO") $ lkup

  rawDatum ←
    liftContractM (msg "Update committee hash UTxO is missing inline datum")
      $ outputDatumDatum tOut.datum
  UpdateCommitteeHashDatum datum ← liftContractM
    (msg "Datum at update committee hash UTxO fromData failed")
    (fromData $ unwrap rawDatum)
  when (datum.committeeHash /= curCommitteeHash)
    (throwContractError "Incorrect committee provided")

  -- Grabbing the last merkle root reference
  -------------------------------------------------------------
  maybePreviousMerkleRoot ← MPTRoot.Utils.findPreviousMptRootTokenUtxo
    previousMerkleRoot
    smrm

  -- Building / submitting the transaction.
  -------------------------------------------------------------
  let
    newDatum = Datum $ toData
      ( UpdateCommitteeHashDatum
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

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (msg "Submitted update committee hash transaction: " <> show txId)
  awaitTxConfirmed txId
  logInfo' (msg "Update committee hash transaction submitted successfully")

  pure txId

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "UpdateCommitteeHash", fun: _ }

-- | `getCommitteeHashPolicy` grabs the committee hash policy and currency symbol
-- | (potentially throwing an error in the case that it is not possible).
getCommitteeHashPolicy ∷
  SidechainParams →
  Contract ()
    { committeeHashPolicy ∷ MintingPolicy
    , committeeHashCurrencySymbol ∷ CurrencySymbol
    }
getCommitteeHashPolicy (SidechainParams sp) = do
  let
    msg = Logging.mkReport
      { mod: "FUELMintingPolicy", fun: "getCommitteeHashPolicy" }
  committeeHashPolicy ← committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: sp.genesisUtxo }
  committeeHashCurrencySymbol ← liftContractM
    (msg "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)
  pure { committeeHashPolicy, committeeHashCurrencySymbol }
