module CommitteCandidateValidator where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  )
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , throwContractError
  )
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(..)
  , PlutusData(Constr)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(..)
  , applyArgs
  , validatorHash
  , validatorHashEnterpriseAddress
  )
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , outputDatumDatum
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, (:))
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Map as Map
import RawScripts (rawCommitteeCandidateValidator)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)
import Types.Scripts (plutusV2Script)

newtype RegisterParams = RegisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PubKey
  , sidechainPubKey ∷ PubKey
  , spoSig ∷ Signature
  , sidechainSig ∷ Signature
  , inputUtxo ∷ TransactionInput
  }

newtype DeregisterParams = DeregisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PubKey
  }

getCommitteeCandidateValidator ∷ SidechainParams → Contract () Validator
getCommitteeCandidateValidator sp = do
  ccvUnapplied ← (plutusV2Script >>> Validator) <$> textEnvelopeBytes
    rawCommitteeCandidateValidator
    PlutusScriptV2
  liftedE (applyArgs ccvUnapplied [ toData sp ])

newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ ByteArray
  , signatures ∷ Array Signature
  , threshold ∷ BigInt.BigInt
  , committeePubKeys ∷
      Array PubKey -- Public keys of all committee members
  }

newtype BlockProducerRegistration = BlockProducerRegistration
  { bprSpoPubKey ∷ PubKey -- own cold verification key hash
  , bprSidechainPubKey ∷ PubKey -- public key in the sidechain's desired format
  , bprSpoSignature ∷ Signature -- Signature of the SPO
  , bprSidechainSignature ∷ Signature -- Signature of the SPO
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
    ) = Constr zero
    [ toData bprSpoPubKey
    , toData bprSidechainPubKey
    , toData bprSpoSignature
    , toData bprSidechainSignature
    , toData bprInputUtxo
    , toData bprOwnPkh
    ]

instance FromData BlockProducerRegistration where
  fromData (Constr n [ a, b, c, d, e, f ])
    | n == zero = BlockProducerRegistration <$>
        ( ( { bprSpoPubKey: _
            , bprSidechainPubKey: _
            , bprSpoSignature: _
            , bprSidechainSignature: _
            , bprInputUtxo: _
            , bprOwnPkh: _
            }
          ) <$> fromData a <*> fromData b <*> fromData c <*> fromData d
            <*> fromData e
            <*> fromData f
        )
  fromData _ = Nothing

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams ∷ SidechainParams
  , bprmSidechainPubKey ∷ String
  , bprmInputUtxo ∷ TransactionInput -- A UTxO that must be spent by the transaction
  }

register ∷ RegisterParams → Contract () Unit
register
  ( RegisterParams
      { sidechainParams
      , spoPubKey
      , sidechainPubKey
      , spoSig
      , sidechainSig
      , inputUtxo
      }
  ) = do
  let msg = report "register"
  ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
  ownAddr ← liftedM (msg "Cannot get own address") getWalletAddress

  ownUtxos ← liftedM (msg "Cannot get own UTxOs") (utxosAt ownAddr)
  validator ← getCommitteeCandidateValidator sidechainParams
  let
    valHash = validatorHash validator
    val = Value.lovelaceValueOf (BigInt.fromInt 1)
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

    constraints ∷ Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput inputUtxo
        <> Constraints.mustPayToScript valHash (Datum (toData datum))
          Constraints.DatumInline
          val
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceAndSignTxE ubTx)
  txId ← submit bsTx
  logInfo' $ msg ("Submitted committeeCandidate register Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' $ msg "register Tx submitted successfully!"

deregister ∷ DeregisterParams → Contract () Unit
deregister (DeregisterParams { sidechainParams, spoPubKey }) = do
  let msg = report "deregister"
  ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
  ownAddr ← liftedM (msg "Cannot get own address") getWalletAddress
  netId ← getNetworkId

  validator ← getCommitteeCandidateValidator sidechainParams
  let valHash = validatorHash validator
  valAddr ← liftContractM (msg "Failed to convert validator hash to an address")
    (validatorHashEnterpriseAddress netId valHash)
  ownUtxos ← liftedM (msg "cannot get UTxOs") (utxosAt ownAddr)
  valUtxos ← liftedM (msg "cannot get val UTxOs") (utxosAt valAddr)
  let valUtxos' = Map.toUnfoldable valUtxos

  ourDatums ← liftAff $ flip parTraverse valUtxos'
    \(input /\ TransactionOutputWithRefScript { output: TransactionOutput out }) →
      runMaybeT $ do
        BlockProducerRegistration datum ← MaybeT $ pure $ (fromData <<< unwrap)
          =<< outputDatumDatum out.datum
        guard (datum.bprSpoPubKey == spoPubKey && ownPkh == datum.bprOwnPkh)
        pure input
  let datums = catMaybes ourDatums

  when (null datums) $ throwContractError
    (msg "Registration utxo cannot be found")

  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs ownUtxos
      <> Lookups.unspentOutputs valUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = mconcat
      ( Constraints.mustBeSignedBy ownPkh :
          (flip Constraints.mustSpendScriptOutput unitRedeemer <$> datums)
      )

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceAndSignTxE ubTx)
  txId ← submit bsTx
  logInfo' $ msg ("Submitted committee deregister Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' $ msg "deregister submitted successfully!"

-- small utility function for error reporting.
report ∷ String → ∀ e. Show e ⇒ e → String
report fn msg = "CommitteeCandidateValidator." <> fn <> ": " <> show msg
