-- | `TrustlessSidechain.CommitteePlainATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build / submit the transaction.
module TrustlessSidechain.CommitteePlainATMSPolicy
  ( ATMSPlainMultisignature(ATMSPlainMultisignature)
  , committeePlainATMSMintFromSidechainParams

  , committeePlainATMS
  , getCommitteePlainATMSPolicy

  , findUpdateCommitteeHashUtxoFromSidechainParams

  , mustMintCommitteePlainATMSPolicy
  , runCommitteePlainATMSPolicy
  ) where

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
  , TransactionInput
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
  )
import Contract.Value as Value
import Data.Bifunctor as Bifunctor
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils as UpdateCommitteeHash.Utils
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging as Logging

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

-- | `committeePlainATMS` grabs the minting policy for the committee plain ATMS
-- | policy
committeePlainATMS ∷ CommitteeCertificateMint → Contract MintingPolicy
committeePlainATMS param = do
  let
    script = decodeTextEnvelope RawScripts.rawCommitteePlainATMSPolicy >>=
      plutusScriptV2FromEnvelope

  unapplied ← Monad.liftContractM "Decoding text envelope failed." script
  applied ← Monad.liftContractE $ Scripts.applyArgs unapplied [ toData param ]
  pure $ PlutusMintingPolicy applied

-- | `getCommitteePlainATMSPolicy` grabs the committee plain ATMS currency
-- | symbol and policy
getCommitteePlainATMSPolicy ∷
  CommitteeCertificateMint →
  Contract
    { committeePlainATMSPolicy ∷ MintingPolicy
    , committeePlainATMSCurrencySymbol ∷ CurrencySymbol
    }
getCommitteePlainATMSPolicy param = do
  let
    msg = report "getCommitteePlainATMSPolicy"
  committeePlainATMSPolicy ← committeePlainATMS param
  committeePlainATMSCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get committee signed token currency symbol")
    (Value.scriptCurrencySymbol committeePlainATMSPolicy)
  pure
    { committeePlainATMSPolicy, committeePlainATMSCurrencySymbol }

-- | `committeePlainATMSMintFromSidechainParams` grabs the `CommitteePlainATMSPolicy`
-- | parameter that corresponds to the given `SidechainParams`
committeePlainATMSMintFromSidechainParams ∷
  SidechainParams → Contract CommitteeCertificateMint
committeePlainATMSMintFromSidechainParams sidechainParams = do
  { committeeOracleCurrencySymbol
  } ← CommitteeOraclePolicy.getCommitteeOraclePolicy sidechainParams
  pure $ CommitteeCertificateMint
    { committeeOraclePolicy: committeeOracleCurrencySymbol
    , thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
    , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
    }

-- | `mustMintCommitteePlainATMSPolicy` provides the constraints to mint a
-- | committee signed token.
-- | Note: this does NOT include a constraint to reference or spend the UTxO
-- | which contains the current committee, so you MUST provide this yourself
-- | afterwards.
mustMintCommitteePlainATMSPolicy ∷
  CommitteeATMSParams (Array (SidechainPublicKey /\ Maybe SidechainSignature)) →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mustMintCommitteePlainATMSPolicy
  ( CommitteeATMSParams
      { currentCommitteeUtxo
      , committeeCertificateMint
      , aggregateSignature: signatures
      , message
      }
  ) = do
  let
    msg = report "mustMintCommitteePlainATMSPolicy"

  let
    messageByteArray = Value.getTokenName message

    -- ensure that the signatures provided are sorted, and do an optimization
    -- to only provide the minimum number of signatures for the onchain code to
    -- validate
    normalizedSignatures = Utils.Crypto.normalizeCommitteePubKeysAndSignatures
      signatures
    curCommitteePubKeys /\ allCurCommitteeSignatures =
      Utils.Crypto.unzipCommitteePubKeysAndSignatures normalizedSignatures
    _ /\ curCommitteeSignatures = Utils.Crypto.takeExactlyEnoughSignatures
      (unwrap committeeCertificateMint).thresholdNumerator
      (unwrap committeeCertificateMint).thresholdDenominator
      (curCommitteePubKeys /\ allCurCommitteeSignatures)
    curCommitteeHash = Utils.Crypto.aggregateKeys curCommitteePubKeys

  -- Grabbing CommitteePlainATMSPolicy
  -------------------------------------------------------------
  { committeePlainATMSPolicy
  } ← getCommitteePlainATMSPolicy committeeCertificateMint

  -- Grabbing the current committee as stored onchain / fail offchain early if
  -- the current committee isn't as expected.
  -------------------------------------------------------------
  let
    { index: committeeORef
    , value:
        committeeTxOut@
          (TransactionOutputWithRefScript { output: TransactionOutput tOut })
    } = currentCommitteeUtxo

  comitteeHashDatum ←
    Monad.liftContractM
      (msg "Update committee UTxO is missing inline datum")
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← Monad.liftContractM
    (msg "Datum at update committee UTxO fromData failed")
    (fromData $ unwrap comitteeHashDatum)

  -- quickly verify that the committee hash matches
  when (datum.aggregatePubKeys /= curCommitteeHash)
    $ Monad.throwContractError
    $ msg "Incorrect committee provided"

  unless
    ( Utils.Crypto.verifyMultiSignature
        ((unwrap committeeCertificateMint).thresholdNumerator)
        ((unwrap committeeCertificateMint).thresholdDenominator)
        curCommitteePubKeys
        (Utils.Crypto.byteArrayToSidechainMessageUnsafe messageByteArray)
        -- this is actually safe because TokenName and SidechainMessage have
        -- the same invariants
        curCommitteeSignatures
    )
    $ Monad.throwContractError
    $ msg
        "Invalid committee signatures for the sidechain message"

  let
    redeemer = Redeemer $ toData $
      ATMSPlainMultisignature
        { currentCommittee: curCommitteePubKeys
        , currentCommitteeSignatures: curCommitteeSignatures
        }

  pure
    { lookups:
        ScriptLookups.unspentOutputs
          (Map.singleton committeeORef committeeTxOut)
          <> ScriptLookups.mintingPolicy committeePlainATMSPolicy
    , constraints:
        TxConstraints.mustMintCurrencyWithRedeemer
          (Scripts.mintingPolicyHash committeePlainATMSPolicy)
          redeemer
          message
          one
    -- Note: we used to include the current committee as reference input
    -- every time, but there are times when one wants to spend the output
    -- with the current committee and hence must provide a redeemer (and
    -- perhaps much more in the transaction!).
    -- So, instead of forcing you to pipe all the data down here, we force
    -- the person calling this function to either include the current committee
    -- as a reference output, or spending the output themselves.
    -- ```
    -- <> TxConstraints.mustReferenceOutput
    -- committeeORef
    -- ```
    }

-- | `runCommitteePlainATMSPolicy` provides a convenient way to submit a
-- | transaction with the constraints given in `mustMintCommitteePlainATMSPolicy`.
-- |
-- | This is mainly just used for testing as one wouldn't want to just call
-- | this in isolation.
-- |
-- | Note: this assumes that the current committee should be given as reference
-- | input (instead of spending it) to make testing a bit more terse.
runCommitteePlainATMSPolicy ∷
  CommitteeATMSParams (Array (SidechainPublicKey /\ Maybe SidechainSignature)) →
  Contract TransactionHash
runCommitteePlainATMSPolicy params = do
  let
    msg = report "runCommitteePlainATMSPolicy"

  mustMintCommitteeATMSPolicyLookupsAndConstraints ←
    mustMintCommitteePlainATMSPolicy params

  let
    extraLookupsAndContraints =
      { lookups: mempty
      , constraints:
          TxConstraints.mustReferenceOutput
            (unwrap params).currentCommitteeUtxo.index
      }

    { lookups, constraints } = mustMintCommitteeATMSPolicyLookupsAndConstraints
      <> extraLookupsAndContraints

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

-- | `findUpdateCommitteeHashUtxoFromSidechainParams` is similar to
-- | `findUpdateCommitteeHashUtxo` (and is indeed a small wrapper over it), but
-- | does the tricky work of grabbing the required currency symbols for you.
findUpdateCommitteeHashUtxoFromSidechainParams ∷
  SidechainParams →
  Contract { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript }
findUpdateCommitteeHashUtxoFromSidechainParams sidechainParams = do
  let -- `mkErr` is used to help generate log messages
    mkErr = report "findUpdateCommitteeHashUtxoFromSidechainParams"

  -- Set up for the committee ATMS schemes
  ------------------------------------
  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy
      sidechainParams
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
        , committeeOraclePolicy: committeeOracleCurrencySymbol
        }

  { committeePlainATMSCurrencySymbol } ← getCommitteePlainATMSPolicy
    committeeCertificateMint

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  merkleRootTokenValidator ← MerkleRoot.Utils.merkleRootTokenValidator
    sidechainParams

  let
    smrm = SignedMerkleRootMint
      { sidechainParams: sidechainParams
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainATMSCurrencySymbol
      , merkleRootValidatorHash: Scripts.validatorHash merkleRootTokenValidator
      }
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    smrm
  merkleRootTokenCurrencySymbol ←
    Monad.liftContractM
      (mkErr "Failed to get merkleRootTokenCurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Build the UpdateCommitteeHash parameter
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
      , merkleRootTokenCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainATMSCurrencySymbol
      }

  -- Finding the current committee
  -------------------------------------------------------------
  lkup ← Monad.liftedM (mkErr "current committee not found") $
    UpdateCommitteeHash.Utils.findUpdateCommitteeHashUtxo uch
  pure lkup

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → String → String
report = Logging.mkReport "CommitteePlainATMSPolicy"
