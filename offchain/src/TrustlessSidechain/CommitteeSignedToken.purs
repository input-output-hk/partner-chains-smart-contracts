-- | `TrustlessSidechain.CommitteeSignedToken` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build the transaction.
module TrustlessSidechain.CommitteeSignedToken where

import Contract.Prelude

import Contract.Log as Log
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , outputDatumDatum
  )
import Contract.Transaction as Transaction
import Contract.TxConstraints
  ( TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  )
import Contract.Value as Value
import Data.Bifunctor as Bifunctor
import Data.Map as Map
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging as Logging

newtype CommitteeSignedTokenMint = CommitteeSignedTokenMint
  { sidechainParams ∷ SidechainParams
  , updateCommitteeHashCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic CommitteeSignedTokenMint _
derive instance Newtype CommitteeSignedTokenMint _
instance ToData CommitteeSignedTokenMint where
  toData
    ( CommitteeSignedTokenMint
        { sidechainParams, updateCommitteeHashCurrencySymbol }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData sidechainParams
      , toData updateCommitteeHashCurrencySymbol
      ]

newtype CommitteeSignedTokenRedeemer = CommitteeSignedTokenRedeemer
  { currentCommittee ∷ Array SidechainPublicKey
  , currentCommitteeSignatures ∷ Array SidechainSignature
  , messageHash ∷ TokenName
  }

derive instance Generic CommitteeSignedTokenRedeemer _
derive instance Newtype CommitteeSignedTokenRedeemer _
instance ToData CommitteeSignedTokenRedeemer where
  toData
    ( CommitteeSignedTokenRedeemer
        { currentCommittee, currentCommitteeSignatures, messageHash }
    ) = Constr (BigNum.fromInt 0)
    [ toData currentCommittee
    , toData currentCommitteeSignatures
    , toData messageHash
    ]

-- | `committeeSignedToken` grabs the minting polciy for the committee signed
-- | token
committeeSignedToken ∷ CommitteeSignedTokenMint → Contract MintingPolicy
committeeSignedToken param = do
  let
    script = decodeTextEnvelope RawScripts.rawCommitteeSignedToken >>=
      plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied [ toData param ]
  pure $ PlutusMintingPolicy applied

-- | `getCommitteeSignedToken` grabs the committee signed token currency symbol
-- | and policy
getCommitteeSignedToken ∷
  CommitteeSignedTokenMint →
  Contract
    { committeeSignedTokenPolicy ∷ MintingPolicy
    , committeeSignedTokenCurrencySymbol ∷ CurrencySymbol
    }
getCommitteeSignedToken param = do
  let
    msg = report "getCommitteeSignedToken"
  committeeSignedTokenPolicy ← committeeSignedToken param
  committeeSignedTokenCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get committee signed token currency symbol")
    (Value.scriptCurrencySymbol committeeSignedTokenPolicy)
  pure
    { committeeSignedTokenPolicy, committeeSignedTokenCurrencySymbol }

-- | `committeeSignedTokenMintFromSidechainParams` grabs the `CommitteeSignedToken`
-- | parameter that corresponds to the given `SidechainParams`
committeeSignedTokenMintFromSidechainParams ∷
  SidechainParams → Contract CommitteeSignedTokenMint
committeeSignedTokenMintFromSidechainParams sidechainParams = do
  { committeeHashCurrencySymbol
  } ← UpdateCommitteeHash.getCommitteeHashPolicy sidechainParams
  pure $ CommitteeSignedTokenMint
    { sidechainParams
    , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
    }

-- | `mustMintCommitteeSignedToken` provides the constraints to mint a
-- | committee signed token [including: the script lookups for this, and the UTxO
-- | for the committee reference input]
mustMintCommitteeSignedToken ∷
  CommitteeSignedTokenMint →
  CommitteeSignedTokenRedeemer →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mustMintCommitteeSignedToken cstm cstr = do
  let
    msg = report "mustMintCommitteeSignedToken"

  -- Unwrapping the provided parameters
  -------------------------------------------------------------
  let
    sidechainParams = (unwrap cstm).sidechainParams
    curCommitteePubKeys = (unwrap cstr).currentCommittee
    msgHash = Value.getTokenName $ (unwrap cstr).messageHash
    curCommitteeSignatures = (unwrap cstr).currentCommitteeSignatures
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys

  -- Grabbing the committee hash currency symbol
  -------------------------------------------------------------
  { committeeHashCurrencySymbol
  , committeeHashTokenName
  } ← UpdateCommitteeHash.getCommitteeHashPolicy sidechainParams

  -- Grabbing the committee signed token currency symbol / minting policy
  -------------------------------------------------------------
  { committeeSignedTokenPolicy
  } ← getCommitteeSignedToken $ CommitteeSignedTokenMint
    { sidechainParams
    , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
    }

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams: sidechainParams
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , uchAssetClass: committeeHashCurrencySymbol /\ committeeHashTokenName
      , merkleRootTokenCurrencySymbol
      }

  -- Grabbing the current committee as stored onchain
  -------------------------------------------------------------
  lkup ← UpdateCommitteeHash.findUpdateCommitteeHashUtxo uch

  { index: committeeHashORef
  , value:
      committeeHashTxOut@
        (TransactionOutputWithRefScript { output: TransactionOutput tOut })
  } ←
    Monad.liftContractM (msg "Failed to find update committee hash UTxO") $ lkup

  comitteeHashDatum ←
    Monad.liftContractM
      (msg "Update committee hash UTxO is missing inline datum")
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← Monad.liftContractM
    (msg "Datum at update committee hash UTxO fromData failed")
    (fromData $ unwrap comitteeHashDatum)

  -- quickly verify that the committee hash matches
  when (datum.committeeHash /= curCommitteeHash)
    $ Monad.throwContractError
    $ msg "Incorrect committee provided"

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap sidechainParams).thresholdNumerator)
        ((unwrap sidechainParams).thresholdDenominator)
        curCommitteePubKeys
        (Utils.Crypto.byteArrayToSidechainMessageUnsafe msgHash)
        -- this is actually safe because TokenName and  the
        -- SidechainMessage have the same invariants (fortunately!)
        curCommitteeSignatures
    )
    $ Monad.throwContractError
    $ msg
        "Invalid committee signatures for the sidechain message"

  let redeemer = Redeemer $ toData cstr

  pure
    { lookups:
        ScriptLookups.unspentOutputs
          (Map.singleton committeeHashORef committeeHashTxOut)
          <> ScriptLookups.mintingPolicy committeeSignedTokenPolicy
    , constraints:
        TxConstraints.mustReferenceOutput
          committeeHashORef
          <> TxConstraints.mustMintCurrencyWithRedeemer
            (Scripts.mintingPolicyHash committeeSignedTokenPolicy)
            redeemer
            (unwrap cstr).messageHash
            one
    }

-- | `runCommitteeSignedToken` provides a convenient way to submit a
-- | transaction with the constraints given in `mustMintCommitteeSignedToken`
-- |
-- | This is mainly just used for testing as one wouldn't want to just call
-- | this in isolation.
runCommitteeSignedToken ∷
  CommitteeSignedTokenMint →
  CommitteeSignedTokenRedeemer →
  Contract TransactionHash
runCommitteeSignedToken cstm cstr = do
  let
    msg = report "runCommitteeSignedToken"

  { lookups, constraints } ← mustMintCommitteeSignedToken cstm cstr

  ubTx ← Monad.liftedE
    ( Bifunctor.lmap (msg <<< show) <$> ScriptLookups.mkUnbalancedTx lookups
        constraints
    )
  bsTx ← Monad.liftedE
    (Bifunctor.lmap (msg <<< show) <$> Transaction.balanceTx ubTx)
  signedTx ← Transaction.signTransaction bsTx
  txId ← Transaction.submit signedTx
  Log.logInfo'
    (msg "Submitted committee signed token transaction: " <> show txId)
  Transaction.awaitTxConfirmed txId
  Log.logInfo' (msg "Committee signed token transaction submitted successfully")

  pure txId

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → String → String
report = Logging.mkReport "CommitteeSignedToken"
