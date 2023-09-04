-- | `TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build / submit the transaction.
-- |
-- | NOTE: this is essentially duplicated from `TrustlessSidechain.CommitteePlainSchnorrEcdsa256k1ATMSPolicy`
module TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy
  ( ATMSPlainSchnorrSecp256k1Multisignature
      ( ATMSPlainSchnorrSecp256k1Multisignature
      )
  , committeePlainSchnorrSecp256k1ATMSMintFromSidechainParams

  , committeePlainSchnorrSecp256k1ATMS
  , getCommitteePlainSchnorrSecp256k1ATMSPolicy

  , findUpdateCommitteeHashUtxoFromSidechainParams

  , mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy
  , runCommitteePlainSchnorrSecp256k1ATMSPolicy
  ) where

import Contract.Prelude

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
  , mkTxUnspentOut
  , outputDatumDatum
  )
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as TxConstraints
import Contract.Value
  ( CurrencySymbol
  )
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils as UpdateCommitteeHash.Utils
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript, InvalidData)
  , OffchainError(InternalError, InvalidInputError)
  )
import TrustlessSidechain.Utils.SchnorrSecp256k1
  ( SchnorrSecp256k1PublicKey
  , SchnorrSecp256k1Signature
  )
import TrustlessSidechain.Utils.SchnorrSecp256k1 as SchnorrSecp256k1
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Versioning.Types
  ( ScriptId(CommitteeOraclePolicy, CommitteeCertificateVerificationPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `ATMSPlainSchnorrSecp256k1Multisignature` corresponds to the onchain type
newtype ATMSPlainSchnorrSecp256k1Multisignature =
  ATMSPlainSchnorrSecp256k1Multisignature
    { currentCommittee ∷ Array SchnorrSecp256k1PublicKey
    , currentCommitteeSignatures ∷ Array SchnorrSecp256k1Signature
    }

derive instance Generic ATMSPlainSchnorrSecp256k1Multisignature _

derive instance Newtype ATMSPlainSchnorrSecp256k1Multisignature _

instance ToData ATMSPlainSchnorrSecp256k1Multisignature where
  toData
    ( ATMSPlainSchnorrSecp256k1Multisignature
        { currentCommittee, currentCommitteeSignatures }
    ) = Constr (BigNum.fromInt 0)
    [ toData currentCommittee
    , toData currentCommitteeSignatures
    ]

-- | `committeePlainSchnorrSecp256k1ATMS` grabs the minting policy for the committee plainSchnorrSecp256k1 ATMS
-- | policy
committeePlainSchnorrSecp256k1ATMS ∷
  { committeeCertificateMint ∷ CommitteeCertificateMint
  , sidechainParams ∷ SidechainParams
  } →
  Contract MintingPolicy
committeePlainSchnorrSecp256k1ATMS { committeeCertificateMint, sidechainParams } =
  do
    let
      script =
        decodeTextEnvelope
          RawScripts.rawCommitteePlainSchnorrSecp256k1ATMSPolicy >>=
          plutusScriptV2FromEnvelope

    versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
    unapplied ← Monad.liftContractM "Decoding text envelope failed." script
    applied ← Monad.liftContractE $ Scripts.applyArgs unapplied
      [ toData committeeCertificateMint, toData versionOracleConfig ]
    pure $ PlutusMintingPolicy applied

-- | `getCommitteePlainSchnorrSecp256k1ATMSPolicy` grabs the committee plainSchnorrSecp256k1 ATMS currency
-- | symbol and policy
getCommitteePlainSchnorrSecp256k1ATMSPolicy ∷
  { committeeCertificateMint ∷ CommitteeCertificateMint
  , sidechainParams ∷ SidechainParams
  } →
  Contract
    { committeePlainSchnorrSecp256k1ATMSPolicy ∷ MintingPolicy
    , committeePlainSchnorrSecp256k1ATMSCurrencySymbol ∷ CurrencySymbol
    }
getCommitteePlainSchnorrSecp256k1ATMSPolicy param = do
  committeePlainSchnorrSecp256k1ATMSPolicy ← committeePlainSchnorrSecp256k1ATMS
    param
  committeePlainSchnorrSecp256k1ATMSCurrencySymbol ← Monad.liftContractM
    ( show $ InternalError $ InvalidScript
        "Failed to get committee plainSchnorrSecp256k1 ATMS currency symbol"
    )
    (Value.scriptCurrencySymbol committeePlainSchnorrSecp256k1ATMSPolicy)
  pure
    { committeePlainSchnorrSecp256k1ATMSPolicy
    , committeePlainSchnorrSecp256k1ATMSCurrencySymbol
    }

-- | `committeePlainSchnorrSecp256k1ATMSMintFromSidechainParams` grabs the `CommitteePlainSchnorrSecp256k1ATMSPolicy`
-- | parameter that corresponds to the given `SidechainParams`
committeePlainSchnorrSecp256k1ATMSMintFromSidechainParams ∷
  SidechainParams → Contract CommitteeCertificateMint
committeePlainSchnorrSecp256k1ATMSMintFromSidechainParams sidechainParams = do
  pure $ CommitteeCertificateMint
    { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
    , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
    }

-- | `mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy` provides the constraints to mint a
-- | committee signed token.
-- | Note: this does NOT include a constraint to reference or spend the UTxO
-- | which contains the current committee, so you MUST provide this yourself
-- | afterwards.
mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy ∷
  { sidechainParams ∷ SidechainParams
  , committeeATMSParams ∷
      CommitteeATMSParams
        (Array (SchnorrSecp256k1PublicKey /\ Maybe SchnorrSecp256k1Signature))
  } →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy
  { committeeATMSParams:
      ( CommitteeATMSParams
          { currentCommitteeUtxo
          , committeeCertificateMint
          , aggregateSignature: signatures
          , message
          }
      )
  , sidechainParams
  } = do
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
    curCommitteeHash = Utils.Crypto.aggregateKeys $ map unwrap
      curCommitteePubKeys

  -- Grabbing CommitteePlainSchnorrSecp256k1ATMSPolicy
  -------------------------------------------------------------
  { committeePlainSchnorrSecp256k1ATMSPolicy
  } ← getCommitteePlainSchnorrSecp256k1ATMSPolicy
    { committeeCertificateMint, sidechainParams }

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
      ( show $ InternalError $ InvalidData
          "Update committee UTxO is missing inline datum"
      )
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← Monad.liftContractM
    ( show $ InternalError $ InvalidData
        "Datum at update committee UTxO fromData failed"
    )
    (fromData $ unwrap comitteeHashDatum)

  -- quickly verify that the committee hash matches
  when (datum.aggregatePubKeys /= curCommitteeHash)
    $ Monad.throwContractError
    $ show
    $ InvalidInputError "Incorrect committee provided"

  unless
    ( Utils.Crypto.verifyMultiSignature
        (\pubKey msg sig → SchnorrSecp256k1.verify sig msg pubKey)
        ((unwrap committeeCertificateMint).thresholdNumerator)
        ((unwrap committeeCertificateMint).thresholdDenominator)
        curCommitteePubKeys
        messageByteArray
        curCommitteeSignatures
    )
    $ Monad.throwContractError
    $ show
    $ InvalidInputError
        "Invalid committee signatures for the sidechain message"

  let
    redeemer = Redeemer $ toData $
      ATMSPlainSchnorrSecp256k1Multisignature
        { currentCommittee: curCommitteePubKeys
        , currentCommitteeSignatures: curCommitteeSignatures
        }

  -- Versioning constraints and lookups

  -- versioning constraints and lookups
  (versioningCommitteeOraclePolicyInput /\ versioningCommitteeOraclePolicyOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1, scriptId: CommitteeOraclePolicy }
      )

  ( committeeCertificateVerificationVersioningInput /\
      committeeCertificateVerificationVersioningOutput
  ) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigInt.fromInt 1
          , scriptId: CommitteeCertificateVerificationPolicy
          }
      )
  let
    versioningConstraints =
      TxConstraints.mustReferenceOutput
        versioningCommitteeOraclePolicyInput
        <> TxConstraints.mustReferenceOutput
          committeeCertificateVerificationVersioningInput
    versioningLookups =
      ScriptLookups.unspentOutputs
        ( Map.singleton versioningCommitteeOraclePolicyInput
            versioningCommitteeOraclePolicyOutput
        )
        <> ScriptLookups.unspentOutputs
          ( Map.singleton committeeCertificateVerificationVersioningInput
              committeeCertificateVerificationVersioningOutput
          )

  pure
    { lookups:
        ScriptLookups.unspentOutputs
          (Map.singleton committeeORef committeeTxOut)
          <> versioningLookups
    , constraints:
        TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (Scripts.mintingPolicyHash committeePlainSchnorrSecp256k1ATMSPolicy)
          redeemer
          message
          one
          ( RefInput $ mkTxUnspentOut
              committeeCertificateVerificationVersioningInput
              committeeCertificateVerificationVersioningOutput
          )
          <> versioningConstraints
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

-- | `runCommitteePlainSchnorrSecp256k1ATMSPolicy` provides a convenient way to submit a
-- | transaction with the constraints given in `mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy`.
-- |
-- | This is mainly just used for testing as one wouldn't want to just call
-- | this in isolation.
-- |
-- | Note: this assumes that the current committee should be given as reference
-- | input (instead of spending it) to make testing a bit more terse.
runCommitteePlainSchnorrSecp256k1ATMSPolicy ∷
  { sidechainParams ∷ SidechainParams
  , committeeATMSParams ∷
      CommitteeATMSParams
        (Array (SchnorrSecp256k1PublicKey /\ Maybe SchnorrSecp256k1Signature))
  } →
  Contract TransactionHash
runCommitteePlainSchnorrSecp256k1ATMSPolicy params = do
  mustMintCommitteeATMSPolicyLookupsAndConstraints ←
    mustMintCommitteePlainSchnorrSecp256k1ATMSPolicy params

  let
    extraLookupsAndContraints =
      { lookups: mempty
      , constraints:
          TxConstraints.mustReferenceOutput
            (unwrap params.committeeATMSParams).currentCommitteeUtxo.index
      }

    { lookups, constraints } = mustMintCommitteeATMSPolicyLookupsAndConstraints
      <> extraLookupsAndContraints

  Utils.Transaction.balanceSignAndSubmit
    "CommitteePlainSchnorrSecp256k1ATMSPolicy"
    lookups
    constraints

-- | `findUpdateCommitteeHashUtxoFromSidechainParams` is similar to
-- | `findUpdateCommitteeHashUtxo` (and is indeed a small wrapper over it), but
-- | does the tricky work of grabbing the required currency symbols for you.
findUpdateCommitteeHashUtxoFromSidechainParams ∷
  SidechainParams →
  Contract { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript }
findUpdateCommitteeHashUtxoFromSidechainParams sidechainParams = do
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
        }

  { committeePlainSchnorrSecp256k1ATMSCurrencySymbol } ←
    getCommitteePlainSchnorrSecp256k1ATMSPolicy
      { committeeCertificateMint, sidechainParams }

  -- minting policy for the merkle root token
  -------------------------------------------------------------

  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    sidechainParams
  merkleRootTokenCurrencySymbol ←
    Monad.liftContractM
      ( show $ InternalError $ InvalidScript
          "Failed to get merkleRootTokenCurrencySymbol"
      )
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  -- Build the UpdateCommitteeHash parameter
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
      , merkleRootTokenCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol:
          committeePlainSchnorrSecp256k1ATMSCurrencySymbol
      }

  -- Finding the current committee
  -------------------------------------------------------------
  lkup ← Monad.liftedM (show $ InvalidInputError $ "current committee not found")
    $
      UpdateCommitteeHash.Utils.findUpdateCommitteeHashUtxo uch
  pure lkup
