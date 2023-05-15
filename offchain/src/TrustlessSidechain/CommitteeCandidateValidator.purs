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
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractE
  , liftContractM
  , liftedE
  , liftedM
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
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , awaitTxConfirmed
  , balanceTx
  , outputDatumDatum
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Alternative (guard)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
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
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)
import TrustlessSidechain.Utils.Logging (class Display, mkReport)

newtype RegisterParams = RegisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ PubKey
  , sidechainPubKey ∷ SidechainPublicKey
  , spoSig ∷ Signature
  , sidechainSig ∷ SidechainSignature
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
  , bprSidechainPubKey ∷ SidechainPublicKey -- public key in the sidechain's desired format
  , bprSpoSignature ∷ Signature -- Signature of the SPO
  , bprSidechainSignature ∷ SidechainSignature -- Signature of the sidechain candidate
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
    ) = Constr (BigNum.fromInt 0)
    [ toData bprSpoPubKey
    , toData bprSidechainPubKey
    , toData bprSpoSignature
    , toData bprSidechainSignature
    , toData bprInputUtxo
    , toData bprOwnPkh
    ]

instance FromData BlockProducerRegistration where
  fromData (Constr n [ a, b, c, d, e, f ]) | n == (BigNum.fromInt 0) =
    { bprSpoPubKey: _
    , bprSidechainPubKey: _
    , bprSpoSignature: _
    , bprSidechainSignature: _
    , bprInputUtxo: _
    , bprOwnPkh: _
    } <$> fromData a <*> fromData b <*> fromData c <*> fromData d <*> fromData e
      <*> fromData f
      <#> BlockProducerRegistration
  fromData _ = Nothing

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams ∷ SidechainParams
  , bprmSidechainPubKey ∷ SidechainPublicKey
  , bprmInputUtxo ∷ TransactionInput -- A UTxO that must be spent by the transaction
  }

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
  let msg = report "register"
  ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
  ownAddr ← liftedM (msg "Cannot get own address") getWalletAddress

  ownUtxos ← utxosAt ownAddr
  validator ← getCommitteeCandidateValidator sidechainParams

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
    valHash = validatorHash validator
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
      <> case maybeCandidatePermissionMintingPolicy of
        Nothing → mempty
        Just { candidatePermissionPolicy } →
          Lookups.mintingPolicy candidatePermissionPolicy

    constraints ∷ Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput inputUtxo
        <> Constraints.mustPayToScript valHash (Datum (toData datum))
          Constraints.DatumInline
          val

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ msg ("Submitted committeeCandidate register Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' $ msg "Register Tx submitted successfully!"

  pure txId

deregister ∷ DeregisterParams → Contract TransactionHash
deregister (DeregisterParams { sidechainParams, spoPubKey }) = do
  let msg = report "deregister"
  ownPkh ← liftedM (msg "Cannot get own pubkey") ownPaymentPubKeyHash
  ownAddr ← liftedM (msg "Cannot get own address") getWalletAddress
  netId ← getNetworkId

  validator ← getCommitteeCandidateValidator sidechainParams
  let valHash = validatorHash validator
  valAddr ← liftContractM (msg "Failed to convert validator hash to an address")
    (validatorHashEnterpriseAddress netId valHash)
  ownUtxos ← utxosAt ownAddr
  valUtxos ← utxosAt valAddr

  ourDatums ← liftAff $ Map.toUnfoldable valUtxos # parTraverse
    \(input /\ TransactionOutputWithRefScript { output: TransactionOutput out }) →
      pure do
        Datum d ← outputDatumDatum out.datum
        BlockProducerRegistration r ← fromData d
        guard (r.bprSpoPubKey == spoPubKey && r.bprOwnPkh == ownPkh)
        pure input
  let datums = catMaybes ourDatums

  when (null datums)
    $ throwContractError (msg "Registration utxo cannot be found")

  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs ownUtxos
      <> Lookups.unspentOutputs valUtxos

    constraints ∷ Constraints.TxConstraints Void Void
    constraints = Constraints.mustBeSignedBy ownPkh
      <> mconcat (flip Constraints.mustSpendScriptOutput unitRedeemer <$> datums)

  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ msg ("Submitted committee deregister Tx: " <> show txId)
  awaitTxConfirmed txId
  logInfo' $ msg "Deregister submitted successfully!"

  pure txId

report ∷ String → (∀ (e ∷ Type). Display e ⇒ e → String)
report = mkReport <<< { mod: "CommitteeCandidateValidator", fun: _ }
