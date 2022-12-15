-- | 'MPTRoot' contains the endpoint functionality for the 'MPTRoot' endpoint
module MPTRoot
  ( module MPTRoot.Types
  , module MPTRoot.Utils
  , saveRoot
  , getMptRootTokenMintingPolicy
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
import Contract.PlutusData (toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Data.Bifunctor (lmap)
import Data.Map as Map
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
import SidechainParams (SidechainParams)
import SidechainParams as SidechainParams
import UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Crypto as Utils.Crypto
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | `getMptRootTokenMintingPolicy` creates the `SignedMerkleRootMint`
-- | parameter from the given sidechain parameters, and calls
-- | `mptRootTokenValidator`
getMptRootTokenMintingPolicy ∷ SidechainParams → Contract () MintingPolicy
getMptRootTokenMintingPolicy sidechainParams = do
  let msg = report "getMptRootTokenMintingPolicy"
  updateCommitteeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy
    $ InitCommitteeHashMint { icTxOutRef: (unwrap sidechainParams).genesisUtxo }
  updateCommitteeHashCurrencySymbol ←
    liftContractM
      (msg "Failed to get updateCommitteeHash CurrencySymbol")
      $ Value.scriptCurrencySymbol updateCommitteeHashPolicy
  mptRootTokenMintingPolicy $ SignedMerkleRootMint
    { sidechainParams
    , updateCommitteeHashCurrencySymbol
    }

-- | 'saveRoot' is the endpoint.
saveRoot ∷ SaveRootParams → Contract () TransactionHash
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

  -- Grab the utxo with the current committee hash / verifying that
  -- this committee has signed the current merkle root (for better error
  -- messages)
  ---------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: updateCommitteeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      , mptRootTokenCurrencySymbol: rootTokenCS
      }
  { index: committeeHashTxIn, value: committeeHashTxOut } ←
    liftedM (msg "Failed to find committee hash utxo") $
      UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  mrimHash ← liftContractM (msg "Failed to create MerkleRootInsertionMessage")
    $ serialiseMrimHash
    $ MerkleRootInsertionMessage
        { sidechainParams: SidechainParams.convertSCParams sidechainParams
        , merkleRoot
        , previousMerkleRoot
        }
  let
    committeePubKeys /\ signatures =
      Utils.Crypto.normalizeCommitteePubKeysAndSignatures committeeSignatures

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap sidechainParams).thresholdNumerator)
        ((unwrap sidechainParams).thresholdDenominator)
        committeePubKeys
        mrimHash
        signatures
    )
    $ throwContractError
    $ msg "Invalid committee signatures for MerkleRootInsertionMessage"

  -- Building the transaction
  ---------------------------------------------------------
  let
    value = Value.singleton rootTokenCS merkleRootTokenName one

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
      <> Lookups.unspentOutputs
        (Map.singleton committeeHashTxIn committeeHashTxOut)
      <> case maybePreviousMerkleRootUtxo of
        Nothing → mempty
        Just { index, value } → Lookups.unspentOutputs (Map.singleton index value)

  -- Submitting the transaction
  ---------------------------------------------------------
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' (msg ("Submitted save root Tx: " <> show txId))
  awaitTxConfirmed txId
  logInfo' (msg "Save root Tx submitted successfully!")

  pure txId

-- | 'report' is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "MPTRoot", fun: _ }
