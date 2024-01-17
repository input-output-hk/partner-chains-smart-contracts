-- | `TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build / submit the transaction.
module TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy
  ( ATMSPlainEcdsaSecp256k1Multisignature(ATMSPlainEcdsaSecp256k1Multisignature)
  , ATMSRedeemer(ATMSMint, ATMSBurn)
  , committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams

  , getCommitteePlainEcdsaSecp256k1ATMSPolicy

  , findUpdateCommitteeHashUtxoFromSidechainParams

  , mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy
  , runCommitteePlainEcdsaSecp256k1ATMSPolicy
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
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , mkTxUnspentOut
  , outputDatumDatum
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Utxos as Utxos
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Value (flattenValue)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, InvalidData, VerificationError)
  )
import TrustlessSidechain.MerkleRoot.Utils as MerkleRoot.Utils
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils as UpdateCommitteeHash.Utils
import TrustlessSidechain.Utils.Address
  ( getCurrencyInfo
  , getCurrencySymbol
  , getOwnWalletAddress
  )
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Utils.Utxos (getOwnUTxOsTotalValue)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CommitteePlainEcdsaSecp256k1ATMSPolicy, MerkleRootTokenPolicy)
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId(CommitteeOraclePolicy, CommitteeCertificateVerificationPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `ATMSPlainEcdsaSecp256k1Multisignature` corresponds to the onchain type
newtype ATMSPlainEcdsaSecp256k1Multisignature =
  ATMSPlainEcdsaSecp256k1Multisignature
    { currentCommittee ∷ Array EcdsaSecp256k1PubKey
    , currentCommitteeSignatures ∷ Array EcdsaSecp256k1Signature
    }

derive instance Generic ATMSPlainEcdsaSecp256k1Multisignature _

derive instance Newtype ATMSPlainEcdsaSecp256k1Multisignature _

instance ToData ATMSPlainEcdsaSecp256k1Multisignature where
  toData
    ( ATMSPlainEcdsaSecp256k1Multisignature
        { currentCommittee, currentCommitteeSignatures }
    ) = Constr (BigNum.fromInt 0)
    [ toData currentCommittee
    , toData currentCommitteeSignatures
    ]

data ATMSRedeemer
  = ATMSMint ATMSPlainEcdsaSecp256k1Multisignature
  | ATMSBurn

derive instance Generic ATMSRedeemer _

instance ToData ATMSRedeemer where
  toData (ATMSMint sig) = Constr (BigNum.fromInt 0) [ toData sig ]
  toData ATMSBurn = Constr (BigNum.fromInt 1) []

-- | `committeePlainEcdsaSecp256k1ATMS` grabs the minting policy for the
-- | committee plainEcdsaSecp256k1 ATMS policy
getCommitteePlainEcdsaSecp256k1ATMSPolicy ∷
  { committeeCertificateMint ∷ CommitteeCertificateMint
  , sidechainParams ∷ SidechainParams
  } →
  Contract CurrencyInfo
getCommitteePlainEcdsaSecp256k1ATMSPolicy
  { committeeCertificateMint, sidechainParams } =
  do
    versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
    getCurrencyInfo CommitteePlainEcdsaSecp256k1ATMSPolicy
      [ toData committeeCertificateMint, toData versionOracleConfig ]

-- | `committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams` grabs the `CommitteePlainEcdsaSecp256k1ATMSPolicy`
-- | parameter that corresponds to the given `SidechainParams`
committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams ∷
  SidechainParams → Contract CommitteeCertificateMint
committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams sidechainParams = do
  pure $ CommitteeCertificateMint
    { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
    , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
    }

-- | `mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy` provides the constraints to mint a
-- | committee signed token.
-- | Note: this does NOT include a constraint to reference or spend the UTxO
-- | which contains the current committee, so you MUST provide this yourself
-- | afterwards.
mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy ∷
  SidechainParams →
  CommitteeATMSParams
    (Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)) →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy
  sidechainParams
  ( CommitteeATMSParams
      { currentCommitteeUtxo
      , committeeCertificateMint
      , aggregateSignature: signatures
      , message
      }
  ) = do
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

  -- Grabbing CommitteePlainEcdsaSecp256k1ATMSPolicy
  -------------------------------------------------------------
  committeePlainEcdsaSecp256k1ATMS ← getCommitteePlainEcdsaSecp256k1ATMSPolicy
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
      ( show $ InvalidData
          "Update committee UTxO is missing inline datum"
      )
      $ outputDatumDatum tOut.datum
  UpdateCommitteeDatum datum ← Monad.liftContractM
    ( show $ InvalidData
        "Datum at update committee UTxO fromData failed"
    )
    (fromData $ unwrap comitteeHashDatum)

  -- quickly verify that the committee hash matches
  when (datum.aggregatePubKeys /= curCommitteeHash)
    $ Monad.throwContractError
    $ show
    $ VerificationError "Incorrect committee provided"

  unless
    ( Utils.Crypto.verifyMultiSignature
        Utils.Crypto.verifyEcdsaSecp256k1Signature
        ((unwrap committeeCertificateMint).thresholdNumerator)
        ((unwrap committeeCertificateMint).thresholdDenominator)
        curCommitteePubKeys
        (Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe messageByteArray)
        -- this is actually safe because TokenName and SidechainMessage have
        -- the same invariants
        curCommitteeSignatures
    )
    $ Monad.throwContractError
    $ show
    $ VerificationError
        "Invalid committee signatures for the sidechain message"

  let
    redeemer = Redeemer $ toData $ ATMSMint $
      ATMSPlainEcdsaSecp256k1Multisignature
        { currentCommittee: curCommitteePubKeys
        , currentCommitteeSignatures: curCommitteeSignatures
        }

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

  ownValue ← getOwnUTxOsTotalValue
  let
    burnWasteTokenConstraints = fold $ do
      (_ /\ tokenName /\ amount) ←
        -- Filtering the entire list is probably suboptimal. If possible this
        -- should be optimised.
        Array.find
          ( \(cs /\ _ /\ _) → cs ==
              committeePlainEcdsaSecp256k1ATMS.currencySymbol
          )
          (flattenValue ownValue)
      pure $
        TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          ( Scripts.mintingPolicyHash
              committeePlainEcdsaSecp256k1ATMS.mintingPolicy
          )
          redeemer
          tokenName
          (negate amount)
          ( RefInput $ mkTxUnspentOut
              committeeCertificateVerificationVersioningInput
              committeeCertificateVerificationVersioningOutput
          )

  ownAddr ← getOwnWalletAddress
  ownUtxos ← Utxos.utxosAt ownAddr

  pure
    { lookups:
        ScriptLookups.unspentOutputs
          (Map.singleton committeeORef committeeTxOut)
          <> versioningLookups
          <> ScriptLookups.unspentOutputs ownUtxos
    , constraints:
        TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          ( Scripts.mintingPolicyHash
              committeePlainEcdsaSecp256k1ATMS.mintingPolicy
          )
          redeemer
          message
          one
          ( RefInput $ mkTxUnspentOut
              committeeCertificateVerificationVersioningInput
              committeeCertificateVerificationVersioningOutput
          )
          <> versioningConstraints
          <> burnWasteTokenConstraints
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

-- | `runCommitteePlainEcdsaSecp256k1ATMSPolicy` provides a convenient way to submit a
-- | transaction with the constraints given in `mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy`.
-- |
-- | This is mainly just used for testing as one wouldn't want to just call
-- | this in isolation.
-- |
-- | Note: this assumes that the current committee should be given as reference
-- | input (instead of spending it) to make testing a bit more terse.
runCommitteePlainEcdsaSecp256k1ATMSPolicy ∷
  SidechainParams →
  CommitteeATMSParams
    (Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)) →
  Contract TransactionHash
runCommitteePlainEcdsaSecp256k1ATMSPolicy sidechainParams params = do
  mustMintCommitteeATMSPolicyLookupsAndConstraints ←
    mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy sidechainParams params

  let
    extraLookupsAndContraints =
      { lookups: mempty
      , constraints:
          TxConstraints.mustReferenceOutput
            (unwrap params).currentCommitteeUtxo.index
      }

  Utils.Transaction.balanceSignAndSubmit "CommitteePlainEcdsaSecp256k1ATMSPolicy"
    ( mustMintCommitteeATMSPolicyLookupsAndConstraints
        <> extraLookupsAndContraints
    )

-- | `findUpdateCommitteeHashUtxoFromSidechainParams` is similar to
-- | `findUpdateCommitteeHashUtxo` (and is indeed a small wrapper over it), but
-- | does the tricky work of grabbing the required currency symbols for you.
findUpdateCommitteeHashUtxoFromSidechainParams ∷
  SidechainParams →
  Contract { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript }
findUpdateCommitteeHashUtxoFromSidechainParams sidechainParams = do
  -- Set up for the committee ATMS schemes
  ------------------------------------
  { currencySymbol: committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy
      sidechainParams
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
        }

  { currencySymbol: committeeCertificateVerificationCurrencySymbol } ←
    getCommitteePlainEcdsaSecp256k1ATMSPolicy
      { committeeCertificateMint, sidechainParams }

  -- Getting the validator / minting policy for the merkle root token
  -------------------------------------------------------------
  -- JSTOLAREK: fix this
  merkleRootTokenMintingPolicy ← MerkleRoot.Utils.merkleRootTokenMintingPolicy
    sidechainParams
  merkleRootTokenCurrencySymbol ←
    getCurrencySymbol MerkleRootTokenPolicy merkleRootTokenMintingPolicy

  -- Build the UpdateCommitteeHash parameter
  -------------------------------------------------------------
  let
    uch = UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol
      , merkleRootTokenCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol
      }

  -- Finding the current committee
  -------------------------------------------------------------
  lkup ← Monad.liftedM
    (show $ NotFoundUtxo "current committee not found")
    (UpdateCommitteeHash.Utils.findUpdateCommitteeHashUtxo uch)
  pure lkup
