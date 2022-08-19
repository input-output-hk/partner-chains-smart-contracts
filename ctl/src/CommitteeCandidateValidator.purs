module CommitteCandidateValidator where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , getNetworkId
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashBaseAddress
  , validatorHashBaseAddress
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(..)
  , PlutusData(Constr)
  , fromData
  , getDatumsByHashes
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( PlutusScript(..)
  , Validator(..)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (TextEnvelopeType(..), textEnvelopeBytes)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, mapMaybe)
import Data.BigInt as BigInt
import Data.Map as Map
import Plutus.Types.Address (Address)
import ScriptsFFI (rawCommitteeCandidateValidator)
import SidechainParams (SidechainParams)

type Signature = String -- Ed25519Signature
type AssetClass = Value.TokenName

newtype RegisterParams = RegisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PaymentPubKeyHash
  , sidechainPubKey ∷ String
  , spoSig ∷ Signature
  , sidechainSig ∷ Signature
  , inputUtxo ∷ TransactionInput
  }

newtype DeregisterParams = DeregisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PaymentPubKeyHash
  }

getCommitteeCandidateValidator ∷ SidechainParams → Contract () Validator
getCommitteeCandidateValidator sp = do
  ccvUnapplied ← (PlutusScript >>> Validator) <$> textEnvelopeBytes
    rawCommitteeCandidateValidator
    PlutusScriptV1
  liftedE (applyArgs ccvUnapplied [ toData sp ])

newtype UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { committeeSignatures ∷
      Array String -- | The current committee's signatures for the 'newCommitteeHash'
  , committeePubKeys ∷
      Array PaymentPubKeyHash -- | 'committeePubKeys' is the current committee public keys
  , newCommitteeHash ∷ String -- | 'newCommitteeHash' is the hash of the new committee
  }

derive instance Generic UpdateCommitteeHashRedeemer _
derive instance Newtype UpdateCommitteeHashRedeemer _
instance ToData UpdateCommitteeHashRedeemer where
  toData
    ( UpdateCommitteeHashRedeemer
        { committeeSignatures, committeePubKeys, newCommitteeHash }
    ) = Constr zero
    [ toData committeeSignatures
    , toData committeePubKeys
    , toData newCommitteeHash
    ]

newtype UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { newCommitteePubKeys ∷
      Array PaymentPubKeyHash -- The public keys of the new committee.
  , token ∷ AssetClass -- The asset class of the NFT for this committee hash
  , committeeSignatures ∷
      Array String -- The signature for the new committee hash.
  , committeePubKeys ∷
      Array PaymentPubKeyHash -- Public keys of the current committee members.
  }

-- | Endpoint parameters for initializing the committee hash
newtype GenesisCommitteeHashParams = GenesisCommitteeHashParams
  { genesisCommitteePubKeys ∷
      Array PaymentPubKeyHash -- Public keys of the initial committee members.
  , genesisAddress ∷ Address -- 'genesisAddress' is the address to spend a utxo to create an NFT.
  , genesisToken ∷ Value.TokenName -- 'genesisToken' is the token name for the NFT
  }

newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ String
  , signatures ∷ Array String
  , threshold ∷ BigInt.BigInt
  , committeePubKeys ∷
      Array PaymentPubKeyHash -- Public keys of all committee members
  }

newtype BlockProducerRegistration = BlockProducerRegistration
  { bprSpoPubKey ∷ PaymentPubKeyHash -- own cold verification key hash
  , bprSidechainPubKey ∷ String -- public key in the sidechain's desired format
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
  , bprmInputUtxo ∷ TransactionOutput -- A UTxO that must be spent by the transaction
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
  ownPkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownStake ← liftedM "Cannot get stakePubKeyHash" ownStakePubKeyHash
  netId ← getNetworkId
  ownAddr ← liftContractM "Cannot get user address" $ payPubKeyHashBaseAddress
    netId
    ownPkh
    ownStake

  ownUtxos ← unwrap <$> liftedM "cannot get UTxOs" (utxosAt ownAddr) -- TrustlessSidechainSchema Text CardanoTx
  validator ← getCommitteeCandidateValidator sidechainParams
  valHash ← liftedM "couldn't get validator hash" $ liftAff
    (validatorHash validator)

  let
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
    constraints = Constraints.mustPayToScript valHash (Datum (toData datum)) val
      <> Constraints.mustSpendPubKeyOutput inputUtxo
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted committeeCandidate register Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "register Tx submitted successfully!"

deregister ∷ DeregisterParams → Contract () Unit
deregister (DeregisterParams { sidechainParams, spoPubKey }) = do
  ownPkh ← liftedM "cannot get own pubkey" ownPaymentPubKeyHash
  ownStake ← liftedM "Cannot get stakePubKeyHash" ownStakePubKeyHash
  netId ← getNetworkId
  ownAddr ← liftContractM "Cannot get user address"
    (payPubKeyHashBaseAddress netId ownPkh ownStake)

  validator ← getCommitteeCandidateValidator sidechainParams
  valHash ← liftedM "couldn't get validator hash"
    (liftAff (validatorHash validator))
  valAddr ← liftContractM "marketPlaceListNft: get validator address"
    (validatorHashBaseAddress netId valHash)
  ownUtxos ← unwrap <$> liftedM "cannot get UTxOs" (utxosAt ownAddr)
  valUtxos ← unwrap <$> liftedM "cannot get val UTxOs" (utxosAt valAddr)
  let valUtxos' = Map.toUnfoldable valUtxos

  datums ← getDatumsByHashes (mapMaybe (snd >>> unwrap >>> _.dataHash) valUtxos')
  ourDatums ← liftAff $ (flip parTraverse) valUtxos' $
    \(input /\ (TransactionOutput out)) → runMaybeT $ do
      BlockProducerRegistration datum ← MaybeT $ pure $ (fromData <<< unwrap)
        =<< (_ `Map.lookup` datums)
        =<< out.dataHash
      guard (datum.bprSpoPubKey == spoPubKey && ownPkh == datum.bprOwnPkh)
      pure input

  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs ownUtxos
      <> Lookups.unspentOutputs valUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = mconcat
      ( (\x → Constraints.mustSpendScriptOutput x unitRedeemer) <$> catMaybes
          ourDatums
      )

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx" (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' ("Submitted committee deregister Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' "deregister submitted successfully!"
