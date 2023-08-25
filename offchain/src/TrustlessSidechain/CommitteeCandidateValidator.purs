module TrustlessSidechain.CommitteeCandidateValidator
  ( RegisterParams(..)
  , DeregisterParams(..)
  , getCommitteeCandidateValidator
  , BlockProducerRegistration(..)
  , BlockProducerRegistrationMsg(..)
  , register
  , deregister
  ) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , getNetworkId
  , validatorHashEnterpriseAddress
  )
import Contract.Monad
  ( Contract
  , liftContractE
  , liftContractM
  , throwContractError
  )
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(Datum)
  , PlutusData(Constr)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), applyArgs, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , outputDatumDatum
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value as Value
import Control.Alternative (guard)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes)
import Data.BigInt as BigInt
import Data.Map as Map
import Record as Record
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  , CandidatePermissionTokenInfo
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.RawScripts (rawCommitteeCandidateValidator)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (PubKey, Signature)
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  )
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript)
  , OffchainError(InternalError, InvalidInputError)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

newtype RegisterParams = RegisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PubKey
  , sidechainPubKey ∷ ByteArray
  , spoSig ∷ Signature
  , sidechainSig ∷ ByteArray
  , inputUtxo ∷ TransactionInput
  , permissionToken ∷ Maybe CandidatePermissionTokenInfo
  }

newtype DeregisterParams = DeregisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PubKey
  }

getCommitteeCandidateValidator ∷ SidechainParams → Contract Validator
getCommitteeCandidateValidator sp = do
  let
    script = decodeTextEnvelope rawCommitteeCandidateValidator >>=
      plutusScriptV2FromEnvelope

  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ applyArgs unapplied [ toData sp ]
  pure $ Validator applied

newtype BlockProducerRegistration = BlockProducerRegistration
  { bprSpoPubKey ∷ PubKey -- own cold verification key hash
  , bprSidechainPubKey ∷ ByteArray -- public key in the sidechain's desired format
  , bprSpoSignature ∷ Signature -- Signature of the SPO
  , bprSidechainSignature ∷ ByteArray -- Signature of the sidechain candidate
  , bprInputUtxo ∷ TransactionInput -- A UTxO that must be spent by the transaction
  , bprOwnPkh ∷ PaymentPubKeyHash -- Owner public key hash
  }

derive instance Generic BlockProducerRegistration _

derive instance Newtype BlockProducerRegistration _

instance ToData BlockProducerRegistration where
  toData
    ( BlockProducerRegistration
        { bprSpoPubKey
        , bprSidechainPubKey
        , bprSpoSignature
        , bprSidechainSignature
        , bprInputUtxo
        , bprOwnPkh
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData bprSpoPubKey
      , toData bprSidechainPubKey
      , toData bprSpoSignature
      , toData bprSidechainSignature
      , toData bprInputUtxo
      , toData bprOwnPkh
      ]

instance FromData BlockProducerRegistration where
  fromData plutusData = case plutusData of
    Constr n [ x1, x2, x3, x4, x5, x6 ] → do
      guard (n == BigNum.fromInt 0)
      x1' ← fromData x1
      x2' ← fromData x2
      x3' ← fromData x3
      x4' ← fromData x4
      x5' ← fromData x5
      x6' ← fromData x6
      pure
        ( BlockProducerRegistration
            { bprSpoPubKey: x1'
            , bprSidechainPubKey: x2'
            , bprSpoSignature: x3'
            , bprSidechainSignature: x4'
            , bprInputUtxo: x5'
            , bprOwnPkh: x6'
            }
        )
    _ → Nothing

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams ∷ SidechainParams
  , bprmSidechainPubKey ∷ ByteArray
  , bprmInputUtxo ∷ TransactionInput -- A UTxO that must be spent by the transaction
  }

-- TODO: we should have some sort of serialization test for this?
-- See: #569 #570
instance ToData BlockProducerRegistrationMsg where
  toData
    ( BlockProducerRegistrationMsg
        { bprmSidechainParams
        , bprmSidechainPubKey
        , bprmInputUtxo
        }
    ) = Constr (BigNum.fromInt 0)
    [ toData bprmSidechainParams
    , toData bprmSidechainPubKey
    , toData bprmInputUtxo
    ]

register ∷ RegisterParams → Contract TransactionHash
register
  ( RegisterParams
      { sidechainParams
      , spoPubKey
      , sidechainPubKey
      , spoSig
      , sidechainSig
      , inputUtxo
      , permissionToken
      }
  ) = do
  netId ← getNetworkId

  ownPkh ← getOwnPaymentPubKeyHash
  ownAddr ← getOwnWalletAddress

  validator ← getCommitteeCandidateValidator sidechainParams
  let valHash = validatorHash validator
  valAddr ← liftContractM
    ( show
        ( InternalError
            (InvalidScript "Couldn't convert validator hash to address")
        )
    )
    (validatorHashEnterpriseAddress netId valHash)

  ownUtxos ← utxosAt ownAddr
  valUtxos ← utxosAt valAddr

  ownRegistrations ← findOwnRegistrations ownPkh spoPubKey valUtxos

  maybeCandidatePermissionMintingPolicy ← case permissionToken of
    Just
      { candidatePermissionTokenUtxo: pUtxo
      , candidatePermissionTokenName: pTokenName
      } →
      map
        ( \rec → Just $ Record.union rec
            { candidatePermissionTokenName: pTokenName }
        )
        $ CandidatePermissionToken.getCandidatePermissionMintingPolicy
        $ CandidatePermissionMint
            { sidechainParams
            , candidatePermissionTokenUtxo: pUtxo
            }
    Nothing → pure Nothing

  let
    val = Value.lovelaceValueOf (BigInt.fromInt 1)
      <> case maybeCandidatePermissionMintingPolicy of
        Nothing → mempty
        Just { candidatePermissionTokenName, candidatePermissionCurrencySymbol } →
          Value.singleton
            candidatePermissionCurrencySymbol
            candidatePermissionTokenName
            one
    datum = BlockProducerRegistration
      { bprSpoPubKey: spoPubKey
      , bprSidechainPubKey: sidechainPubKey
      , bprSpoSignature: spoSig
      , bprSidechainSignature: sidechainSig
      , bprInputUtxo: inputUtxo
      , bprOwnPkh: ownPkh
      }

    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs ownUtxos
      <> Lookups.validator validator
      <> Lookups.unspentOutputs valUtxos
      <> case maybeCandidatePermissionMintingPolicy of
        Nothing → mempty
        Just { candidatePermissionPolicy } →
          Lookups.mintingPolicy candidatePermissionPolicy

    constraints ∷ Constraints.TxConstraints Void Void
    constraints =
      -- Sending new registration to validator address
      Constraints.mustSpendPubKeyOutput inputUtxo
        <> Constraints.mustPayToScript valHash (Datum (toData datum))
          Constraints.DatumInline
          val

        -- Consuming old registration UTxOs
        <> Constraints.mustBeSignedBy ownPkh
        <> mconcat
          ( flip Constraints.mustSpendScriptOutput unitRedeemer <$>
              ownRegistrations
          )

  balanceSignAndSubmit "Registers Committee Candidate" lookups constraints

deregister ∷ DeregisterParams → Contract TransactionHash
deregister (DeregisterParams { sidechainParams, spoPubKey }) = do
  ownPkh ← getOwnPaymentPubKeyHash
  ownAddr ← getOwnWalletAddress
  netId ← getNetworkId

  validator ← getCommitteeCandidateValidator sidechainParams
  let valHash = validatorHash validator
  valAddr ← liftContractM
    ( show
        ( InternalError
            (InvalidScript "Couldn't convert validator hash to address")
        )
    )
    (validatorHashEnterpriseAddress netId valHash)
  ownUtxos ← utxosAt ownAddr
  valUtxos ← utxosAt valAddr

  ownRegistrations ← findOwnRegistrations ownPkh spoPubKey valUtxos

  when (null ownRegistrations)
    $ throwContractError
        (InvalidInputError "Couldn't find registration UTxO")

  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs ownUtxos
      <> Lookups.unspentOutputs valUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustBeSignedBy ownPkh
      <> mconcat
        (flip Constraints.mustSpendScriptOutput unitRedeemer <$> ownRegistrations)

  balanceSignAndSubmit "Deregister Committee Candidate" lookups constraints

-- | Based on the wallet public key hash and the SPO public key, it finds the
-- | the registration UTxOs of the committee member/candidate
findOwnRegistrations ∷
  PaymentPubKeyHash →
  PubKey →
  UtxoMap →
  Contract (Array TransactionInput)
findOwnRegistrations ownPkh spoPubKey validatorUtxos = do
  mayTxIns ← Map.toUnfoldable validatorUtxos #
    parTraverse
      \(input /\ TransactionOutputWithRefScript { output: TransactionOutput out }) →
        pure do
          Datum d ← outputDatumDatum out.datum
          BlockProducerRegistration r ← fromData d
          guard (r.bprSpoPubKey == spoPubKey && r.bprOwnPkh == ownPkh)
          pure input
  pure $ catMaybes mayTxIns
