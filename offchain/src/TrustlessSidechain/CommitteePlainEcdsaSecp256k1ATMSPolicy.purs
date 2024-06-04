-- | `TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy` provides functionality to
-- | replicate the onchain code data types, and grab its minting policies and
-- | currency symbols.
-- | Also, we provide mechanisms for creating the lookups and constraints to
-- | build / submit the transaction.
module TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy
  ( ATMSPlainEcdsaSecp256k1Multisignature(ATMSPlainEcdsaSecp256k1Multisignature)
  , ATMSRedeemer(ATMSMint, ATMSBurn)
  , committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams

  , committeePlainEcdsaSecp256k1ATMSCurrencyInfo

  , findUpdateCommitteeHashUtxoFromSidechainParams

  , mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy
  , runCommitteePlainEcdsaSecp256k1ATMSPolicy
  ) where

import Contract.Prelude

import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.Int as Int
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.Value as Value
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , RedeemerDatum(RedeemerDatum)
  , fromData
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Data.Array as Array
import Data.Map as Map
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT, throw)
import Run.Except as Run
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, InvalidData, VerificationError)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils as UpdateCommitteeHash.Utils
import TrustlessSidechain.Utils.Address
  ( getCurrencyInfo
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
  ( ScriptId(CommitteePlainEcdsaSecp256k1ATMSPolicy)
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId(CommitteeOraclePolicy, CommitteeCertificateVerificationPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | `ATMSPlainEcdsaSecp256k1Multisignature` corresponds to the onchain type
newtype ATMSPlainEcdsaSecp256k1Multisignature =
  ATMSPlainEcdsaSecp256k1Multisignature
    { currentCommittee ∷ Array EcdsaSecp256k1PubKey
    , currentCommitteeSignatures ∷ Array EcdsaSecp256k1Signature
    }

derive instance Eq ATMSPlainEcdsaSecp256k1Multisignature

derive instance Generic ATMSPlainEcdsaSecp256k1Multisignature _

instance Show ATMSPlainEcdsaSecp256k1Multisignature where
  show = genericShow

derive instance Newtype ATMSPlainEcdsaSecp256k1Multisignature _

instance ToData ATMSPlainEcdsaSecp256k1Multisignature where
  toData
    ( ATMSPlainEcdsaSecp256k1Multisignature
        { currentCommittee, currentCommitteeSignatures }
    ) = Constr (BigNum.fromInt 0)
    [ toData currentCommittee
    , toData currentCommitteeSignatures
    ]

instance FromData ATMSPlainEcdsaSecp256k1Multisignature where
  fromData = case _ of
    Constr tag [ t1, t2 ] | tag == BigNum.fromInt 0 → do
      currentCommittee ← fromData t1
      currentCommitteeSignatures ← fromData t2
      pure $ ATMSPlainEcdsaSecp256k1Multisignature
        { currentCommittee, currentCommitteeSignatures }
    _ → Nothing

data ATMSRedeemer
  = ATMSMint ATMSPlainEcdsaSecp256k1Multisignature
  | ATMSBurn

derive instance Eq ATMSRedeemer

derive instance Generic ATMSRedeemer _

instance Show ATMSRedeemer where
  show = genericShow

instance ToData ATMSRedeemer where
  toData (ATMSMint sig) = Constr (BigNum.fromInt 0) [ toData sig ]
  toData ATMSBurn = Constr (BigNum.fromInt 1) []

instance FromData ATMSRedeemer where
  fromData = case _ of
    Constr tag [ t1 ] | tag == BigNum.fromInt 0 → do
      sig ← fromData t1
      pure $ ATMSMint sig
    Constr tag [] | tag == BigNum.fromInt 1 → pure ATMSBurn
    _ → Nothing

-- | `committeePlainEcdsaSecp256k1ATMS` grabs the minting policy for the
-- | committee plainEcdsaSecp256k1 ATMS policy
committeePlainEcdsaSecp256k1ATMSCurrencyInfo ∷
  ∀ r.
  { committeeCertificateMint ∷ CommitteeCertificateMint
  , sidechainParams ∷ SidechainParams
  } →
  Run (EXCEPT OffchainError + WALLET + r) CurrencyInfo
committeePlainEcdsaSecp256k1ATMSCurrencyInfo
  { committeeCertificateMint, sidechainParams } =
  do
    versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
    getCurrencyInfo CommitteePlainEcdsaSecp256k1ATMSPolicy
      [ toData committeeCertificateMint, toData versionOracleConfig ]

-- | `committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams` grabs the `CommitteePlainEcdsaSecp256k1ATMSPolicy`
-- | parameter that corresponds to the given `SidechainParams`
committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams ∷
  SidechainParams → CommitteeCertificateMint
committeePlainEcdsaSecp256k1ATMSMintFromSidechainParams sidechainParams =
  CommitteeCertificateMint
    { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
    , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
    }

-- | `mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy` provides the constraints to mint a
-- | committee signed token.
-- | Note: this does NOT include a constraint to reference or spend the UTxO
-- | which contains the current committee, so you MUST provide this yourself
-- | afterwards.
mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy ∷
  ∀ r.
  SidechainParams →
  CommitteeATMSParams
    (Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)) →
  Run (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    { lookups ∷ ScriptLookups, constraints ∷ TxConstraints }
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
    messageByteArray = unAssetName message

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
    curCommitteeHash = unsafePartial $ Utils.Crypto.aggregateKeys $ map unwrap
      curCommitteePubKeys

  -- Grabbing CommitteePlainEcdsaSecp256k1ATMSPolicy
  -------------------------------------------------------------
  committeePlainEcdsaSecp256k1ATMS ← committeePlainEcdsaSecp256k1ATMSCurrencyInfo
    { committeeCertificateMint, sidechainParams }

  -- Grabbing the current committee as stored onchain / fail offchain early if
  -- the current committee isn't as expected.
  -------------------------------------------------------------
  let
    { index: committeeORef
    , value:
        committeeTxOut@
          (TransactionOutput tOut)
    } = currentCommitteeUtxo

  comitteeHashDatum ←
    Run.note
      ( InvalidData
          "Update committee UTxO is missing inline datum"
      )
      $ tOut.datum
      >>= outputDatumDatum
  UpdateCommitteeDatum datum ← Run.note
    ( InvalidData
        "Datum at update committee UTxO fromData failed"
    )
    (fromData comitteeHashDatum)

  -- quickly verify that the committee hash matches
  Effect.logInfo' (show curCommitteeHash)
  when (datum.aggregatePubKeys /= curCommitteeHash)
    $ throw
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
    $ throw
    $ VerificationError
        "Invalid committee signatures for the sidechain message"

  let
    redeemer = RedeemerDatum $ toData $ ATMSMint $
      ATMSPlainEcdsaSecp256k1Multisignature
        { currentCommittee: curCommitteePubKeys
        , currentCommitteeSignatures: curCommitteeSignatures
        }

  -- versioning constraints and lookups
  (versioningCommitteeOraclePolicyInput /\ versioningCommitteeOraclePolicyOutput) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1, scriptId: CommitteeOraclePolicy }
      )

  ( committeeCertificateVerificationVersioningInput /\
      committeeCertificateVerificationVersioningOutput
  ) ←
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { version: BigNum.fromInt 1
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
          (MultiAsset.flatten $ Value.getMultiAsset ownValue)
      mintAmount ← Int.fromBigInt $ negate $ BigNum.toBigInt amount
      pure $
        TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          ( PlutusScript.hash
              committeePlainEcdsaSecp256k1ATMS.mintingPolicy
          )
          redeemer
          tokenName
          mintAmount
          ( RefInput $ TransactionUnspentOutput
              { input: committeeCertificateVerificationVersioningInput
              , output: committeeCertificateVerificationVersioningOutput
              }
          )

  ownAddr ← getOwnWalletAddress
  ownUtxos ← Effect.utxosAt ownAddr

  pure
    { lookups:
        ScriptLookups.unspentOutputs
          (Map.singleton committeeORef committeeTxOut)
          <> versioningLookups
          <> ScriptLookups.unspentOutputs ownUtxos
    , constraints:
        TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          ( PlutusScript.hash
              committeePlainEcdsaSecp256k1ATMS.mintingPolicy
          )
          redeemer
          message
          (Int.fromInt 1)
          ( RefInput $ TransactionUnspentOutput
              { input: committeeCertificateVerificationVersioningInput
              , output: committeeCertificateVerificationVersioningOutput
              }
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
  ∀ r.
  SidechainParams →
  CommitteeATMSParams
    (Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)) →
  Run (APP + r) TransactionHash
runCommitteePlainEcdsaSecp256k1ATMSPolicy sidechainParams params = do
  mustMintCommitteeATMSPolicyLookupsAndConstraints ←
    mustMintCommitteePlainEcdsaSecp256k1ATMSPolicy sidechainParams params
  Effect.logInfo'
    ( show
        (unwrap params).currentCommitteeUtxo.value
    )
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
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { index ∷ TransactionInput, value ∷ TransactionOutput }
findUpdateCommitteeHashUtxoFromSidechainParams sidechainParams = do
  -- Finding the current committee
  -------------------------------------------------------------
  lkup ← Effect.fromMaybeThrow
    (NotFoundUtxo "current committee not found")
    (UpdateCommitteeHash.Utils.findUpdateCommitteeHashUtxo sidechainParams)
  pure lkup
