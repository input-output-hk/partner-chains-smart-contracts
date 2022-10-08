-- | 'MPTRoot' contains the endpoint functionality for the 'MPTRoot' endpoint
module MPTRoot
  ( module MPTRoot.Types
  , module MPTRoot.Utils
  , saveRoot
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import MPTRoot.Types
  ( MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SaveRootParams(SaveRootParams)
  , SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  )
import MPTRoot.Utils
  ( findMptRootTokenUtxo
  , findPreviousMptRootTokenUtxo
  , mptRootTokenMintingPolicy
  , mptRootTokenValidator
  , serialiseMrimHash
  )
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | 'saveRoot' is the endpoint.
saveRoot ∷ SaveRootParams → Contract () Unit
saveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, previousMerkleRoot, committeeSignatures }
  ) = do
  let msg = report "saveRoot"

  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      (msg "Failed to get updateCommitteeHash CurrencySymbol")
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol
      }
  rootTokenMP ← mptRootTokenMintingPolicy smrm
  rootTokenCS ←
    liftContractM (msg "Cannot get CurrencySymbol of mptRootTokenMintingPolicy")
      $ Value.scriptCurrencySymbol rootTokenMP
  rootTokenVal ← mptRootTokenValidator sidechainParams
  merkleRootTokenName ←
    liftContractM
      (msg "Invalid merkle root TokenName for mptRootTokenMintingPolicy")
      $ Value.mkTokenName merkleRoot

  -- Grab the transaction holding the last merkle root
  ---------------------------------------------------------
  maybePreviousMerkleRootUtxo ← findPreviousMptRootTokenUtxo previousMerkleRoot
    smrm

  -- Grab the utxo with the current committee hash
  ---------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: updateCommitteeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      , mptRootTokenCurrencySymbol: rootTokenCS
      }
  { index: committeeHashTxIn, value: _committeeHashTxOut } ←
    liftedM (msg "Failed to find committee hash utxo") $
      UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  -- Building the transaction
  ---------------------------------------------------------
  let
    value = Value.singleton rootTokenCS merkleRootTokenName one
    committeePubKeys /\ signatures =
      Utils.Crypto.normalizeCommitteePubKeysAndSignatures committeeSignatures

    redeemer = SignedMerkleRoot
      { merkleRoot, previousMerkleRoot, signatures, committeePubKeys }

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer (wrap (toData redeemer)) value
        <> TxConstraints.mustPayToScript (Scripts.validatorHash rootTokenVal)
          unitDatum
          TxConstraints.DatumWitness
          value
        <> TxConstraints.mustReferenceOutput committeeHashTxIn
        <> case maybePreviousMerkleRootUtxo of
          Nothing → mempty
          Just { index: oref } → TxConstraints.mustReferenceOutput oref

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy rootTokenMP

  -- Submitting the transaction
  ---------------------------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM (msg "Failed to balance/sign tx") $ balanceAndSignTx ubTx
  txId ← submit bsTx
  logInfo' (msg ("Submitted save root Tx: " <> show txId))
  awaitTxConfirmed txId
  logInfo' (msg "Save root Tx submitted successfully!")

-- | 'report' is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "MPTRoot", fun: _ }
