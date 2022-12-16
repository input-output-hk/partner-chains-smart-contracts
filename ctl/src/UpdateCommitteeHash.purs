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
import Data.Array as Array
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
updateCommitteeHash (UpdateCommitteeHashParams uchp) = do
  let -- `msg` is used to help generate log messages
    msg = report "updateCommitteeHash"

  -- Getting the minting policy / currency symbol / token name for update
  -- committee hash
  -------------------------------------------------------------
  pol ← committeeHashPolicy
    ( InitCommitteeHashMint
        { icTxOutRef: (\(SidechainParams x) → x.genesisUtxo) uchp.sidechainParams
        }
    )

  cs ← liftContractM (msg "Failed to get updateCommitteeHash minting policy")
    $ Value.scriptCurrencySymbol pol

  let tn = initCommitteeHashMintTn

  -- Getting the minting policy for the mpt root token
  -------------------------------------------------------------
  let
    smrm = SignedMerkleRootMint
      { sidechainParams: uchp.sidechainParams
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
  when (null uchp.committeeSignatures)
    (throwContractError $ msg "Empty Committee")

  let
    newCommitteeSorted = Array.sort uchp.newCommitteePubKeys
    newCommitteeHash = aggregateKeys newCommitteeSorted

    curCommitteePubKeys /\ committeeSignatures =
      Utils.Crypto.normalizeCommitteePubKeysAndSignatures uchp.committeeSignatures
    curCommitteeHash = aggregateKeys curCommitteePubKeys

  uchmsg ← liftContractM (msg "Failed to get update committee hash message")
    $ serialiseUchmHash
    $ UpdateCommitteeHashMessage
        { sidechainParams: SidechainParams.convertSCParams uchp.sidechainParams
        , newCommitteePubKeys: newCommitteeSorted
        , previousMerkleRoot: uchp.previousMerkleRoot
        , sidechainEpoch: uchp.sidechainEpoch
        }

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap uchp.sidechainParams).thresholdNumerator)
        ((unwrap uchp.sidechainParams).thresholdDenominator)
        curCommitteePubKeys
        uchmsg
        committeeSignatures
    )
    ( throwContractError $ msg
        "Invalid committee signatures for UpdateCommitteeHashMessage"
    )

  -- Getting the validator / building the validator hash
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams: uchp.sidechainParams
      , uchAssetClass: assetClass cs tn
      , mptRootTokenCurrencySymbol
      }
  updateValidator ← updateCommitteeHashValidator uch
  let valHash = Scripts.validatorHash updateValidator

  -- Grabbing the old committee / verifying that it really is the old committee
  -------------------------------------------------------------
  lkup ← findUpdateCommitteeHashUtxo uch
  { index: oref
  , value: (TransactionOutputWithRefScript { output: TransactionOutput tOut })
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
    uchp.previousMerkleRoot
    smrm

  -- Building / submitting the transaction.
  -------------------------------------------------------------
  let
    newDatum = Datum $ toData
      ( UpdateCommitteeHashDatum
          { committeeHash: newCommitteeHash, sidechainEpoch: uchp.sidechainEpoch }
      )
    value = assetClassValue (unwrap uch).uchAssetClass one
    redeemer = Redeemer $ toData
      ( UpdateCommitteeHashRedeemer
          { committeeSignatures
          , committeePubKeys: curCommitteePubKeys
          , newCommitteePubKeys: newCommitteeSorted
          , previousMerkleRoot: uchp.previousMerkleRoot
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( Map.singleton oref
            ( TransactionOutputWithRefScript
                { output: TransactionOutput tOut, scriptRef: Nothing }
            )
        )
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
