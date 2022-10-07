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
  ( SaveRootParams(SaveRootParams)
  , SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  )
import MPTRoot.Utils
  ( findLastMptRootTokenUtxo
  , findMptRootTokenUtxo
  , mptRootTokenMintingPolicy
  , mptRootTokenValidator
  )
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto

-- | 'saveRoot' is the endpoint.
saveRoot ∷ SaveRootParams → Contract () Unit
saveRoot
  ( SaveRootParams
      { sidechainParams, merkleRoot, lastMerkleRoot, committeeSignatures }
  ) = do

  -- Getting the required validators / minting policies...
  ---------------------------------------------------------
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      "error 'saveRoot': failed to get update committee hash currency symbol"
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  let
    smrm = SignedMerkleRootMint
      { sidechainParams
      , updateCommitteeHashCurrencySymbol
      }
  rootTokenMP ← mptRootTokenMintingPolicy smrm
  rootTokenCS ← liftContractM "Cannot get currency symbol"
    (Value.scriptCurrencySymbol rootTokenMP)
  rootTokenVal ← mptRootTokenValidator sidechainParams
  merkleRootTokenName ← liftContractM
    "error 'saveRoot': invalid merkle root token name"
    (Value.mkTokenName merkleRoot)

  -- Grab the transaction holding the last merkle root
  ---------------------------------------------------------
  maybeLastMerkleRootUtxo ← findLastMptRootTokenUtxo lastMerkleRoot smrm

  -- Grab the utxo with the current committee hash
  ---------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: updateCommitteeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      }
  { index: committeeHashTxIn, value: _committeeHashTxOut } ←
    liftedM "error 'saveRoot': failed to find committee hash utxo" $
      UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  -- Building the transaction
  ---------------------------------------------------------
  let
    value = Value.singleton rootTokenCS merkleRootTokenName one
    committeePubKeys /\ signatures =
      Utils.Crypto.normalizeCommitteePubKeysAndSignatures committeeSignatures

    redeemer = SignedMerkleRoot
      { merkleRoot, lastMerkleRoot, signatures, committeePubKeys }

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer (wrap (toData redeemer)) value
        <> TxConstraints.mustPayToScript (Scripts.validatorHash rootTokenVal)
          unitDatum
          TxConstraints.DatumWitness
          value
        <> TxConstraints.mustReferenceOutput committeeHashTxIn
        <> case maybeLastMerkleRootUtxo of
          Nothing → mempty
          Just { index: oref } → TxConstraints.mustReferenceOutput oref

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy rootTokenMP

  -- Submitting the transaction
  ---------------------------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted saveRoot Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "saveRoot Tx submitted successfully!"
