-- | `TrustlessSidechain.CommitteePlainATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build the transaction.
module TrustlessSidechain.CommitteePlainATMSPolicy where

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
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
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
import Data.BigInt (BigInt)
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

-- | `CommitteePlainATMSParams` is a type to bundle up all the required data
-- | when building the transaction (this is only used offchain)
newtype CommitteePlainATMSParams = CommitteePlainATMSParams
  {
    -- UTxO for the current committee as stored onchain (which should be
    -- uniquely identified by the token
    -- `CommitteeCertificateMint.committeeOraclePolicy`).
    currentCommitteeUtxo ∷
      { index ∷ TransactionInput
      , value ∷ TransactionOutputWithRefScript
      }
  , -- parameter for the onchain code
    committeeCertificateMint ∷ CommitteeCertificateMint
  , -- signatures for the below message
    atmsPlainMultisignature ∷ ATMSPlainMultisignature
  ,
    -- the message that should be signed (note: this *must* be a token name
    -- so we have the usual size restrictions of a token name i.e., you
    -- probably want this to be the hash of the message you wish to sign)
    message ∷ TokenName
  }

-- | `CommitteeCertificateMint` corresponds to the onchain type that is used to
-- | parameterize a committee certificate verification minting policy.
newtype CommitteeCertificateMint = CommitteeCertificateMint
  { committeeOraclePolicy ∷ CurrencySymbol
  , thresholdNumerator ∷ BigInt
  , thresholdDenominator ∷ BigInt
  }

instance ToData CommitteeCertificateMint where
  toData
    ( CommitteeCertificateMint
        { committeeOraclePolicy, thresholdNumerator, thresholdDenominator }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData committeeOraclePolicy
      , toData thresholdNumerator
      , toData thresholdDenominator
      ]

derive instance Generic CommitteeCertificateMint _
derive instance Newtype CommitteeCertificateMint _

-- | `ATMSPlainMultisignature` corresponds to the onchain type
newtype ATMSPlainMultisignature = ATMSPlainMultisignature
  { currentCommittee ∷ Array SidechainPublicKey
  , currentCommitteeSignatures ∷ Array SidechainSignature
  }

derive instance Generic ATMSPlainMultisignature _
derive instance Newtype ATMSPlainMultisignature _
instance ToData ATMSPlainMultisignature where
  toData
    ( ATMSPlainMultisignature
        { currentCommittee, currentCommitteeSignatures }
    ) = Constr (BigNum.fromInt 0)
    [ toData currentCommittee
    , toData currentCommitteeSignatures
    ]

-- | `committeeSignedToken` grabs the minting polciy for the committee signed
-- | token
committeeSignedToken ∷ CommitteeCertificateMint → Contract MintingPolicy
committeeSignedToken param = do
  let
    script = decodeTextEnvelope RawScripts.rawCommitteePlainATMSPolicy >>=
      plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied [ toData param ]
  pure $ PlutusMintingPolicy applied

-- | `getCommitteePlainATMSPolicy` grabs the committee signed token currency symbol
-- | and policy
getCommitteePlainATMSPolicy ∷
  CommitteeCertificateMint →
  Contract
    { committeeSignedTokenPolicy ∷ MintingPolicy
    , committeeSignedTokenCurrencySymbol ∷ CurrencySymbol
    }
getCommitteePlainATMSPolicy param = do
  let
    msg = report "getCommitteePlainATMSPolicy"
  committeeSignedTokenPolicy ← committeeSignedToken param
  committeeSignedTokenCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get committee signed token currency symbol")
    (Value.scriptCurrencySymbol committeeSignedTokenPolicy)
  pure
    { committeeSignedTokenPolicy, committeeSignedTokenCurrencySymbol }

-- | `committeeSignedTokenMintFromSidechainParams` grabs the `CommitteePlainATMSPolicy`
-- | parameter that corresponds to the given `SidechainParams`
committeeSignedTokenMintFromSidechainParams ∷
  SidechainParams → Contract CommitteeCertificateMint
committeeSignedTokenMintFromSidechainParams sidechainParams = do
  { committeeHashCurrencySymbol
  } ← UpdateCommitteeHash.getCommitteeHashPolicy sidechainParams
  pure $ CommitteeCertificateMint
    { committeeOraclePolicy: committeeHashCurrencySymbol
    , thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
    , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
    }

-- | `mustMintCommitteePlainATMSPolicy` provides the constraints to mint a
-- | committee signed token (including: the script lookups for this, and the UTxO
-- | for the committee reference input)
mustMintCommitteePlainATMSPolicy ∷
  CommitteePlainATMSParams →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mustMintCommitteePlainATMSPolicy
  ( CommitteePlainATMSParams
      { currentCommitteeUtxo
      , committeeCertificateMint
      , atmsPlainMultisignature
      , message
      }
  ) = do
  let
    msg = report "mustMintCommitteePlainATMSPolicy"

  -- Unwrapping the provided parameters
  -------------------------------------------------------------
  let
    curCommitteePubKeys = (unwrap atmsPlainMultisignature).currentCommittee
    msgHash = Value.getTokenName message
    curCommitteeSignatures =
      (unwrap atmsPlainMultisignature).currentCommitteeSignatures
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys

  -- Grabbing CommitteePlainATMSPolicy
  -------------------------------------------------------------
  { committeeSignedTokenPolicy
  , committeeSignedTokenCurrencySymbol
  } ← getCommitteePlainATMSPolicy committeeCertificateMint

  -- Grabbing the current committee as stored onchain
  -------------------------------------------------------------
  let
    { index: committeeHashORef
    , value:
        committeeHashTxOut@
          (TransactionOutputWithRefScript { output: TransactionOutput tOut })
    } = currentCommitteeUtxo

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
        ((unwrap committeeCertificateMint).thresholdNumerator)
        ((unwrap committeeCertificateMint).thresholdDenominator)
        curCommitteePubKeys
        (Utils.Crypto.byteArrayToSidechainMessageUnsafe msgHash)
        -- this is actually safe because TokenName and  the
        -- SidechainMessage have the same invariants (fortunately!)
        curCommitteeSignatures
    )
    $ Monad.throwContractError
    $ msg
        "Invalid committee signatures for the sidechain message"

  let redeemer = Redeemer $ toData atmsPlainMultisignature

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
            message
            one
    }

-- | `runCommitteePlainATMSPolicy` provides a convenient way to submit a
-- | transaction with the constraints given in `mustMintCommitteePlainATMSPolicy`
-- |
-- | This is mainly just used for testing as one wouldn't want to just call
-- | this in isolation.
runCommitteePlainATMSPolicy ∷
  CommitteePlainATMSParams →
  Contract TransactionHash
runCommitteePlainATMSPolicy params = do
  let
    msg = report "runCommitteePlainATMSPolicy"

  { lookups, constraints } ← mustMintCommitteePlainATMSPolicy params

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
report = Logging.mkReport "CommitteePlainATMSPolicy"
