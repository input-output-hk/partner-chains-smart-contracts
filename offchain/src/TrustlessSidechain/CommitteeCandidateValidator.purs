module TrustlessSidechain.CommitteeCandidateValidator
  ( RegisterParams(..)
  , DeregisterParams(..)
  , getCommitteeCandidateValidator
  , BlockProducerRegistration(..)
  , BlockProducerRegistrationMsg(..)
  , StakeOwnership(..)
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
import Contract.Scripts (Validator, validatorHash)
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
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

newtype RegisterParams = RegisterParams
  { sidechainParams ∷ SidechainParams
  , stakeOwnership ∷ StakeOwnership
  , sidechainPubKey ∷ ByteArray
  , sidechainSig ∷ ByteArray
  , inputUtxo ∷ TransactionInput
  , permissionToken ∷ Maybe CandidatePermissionTokenInfo
  }

newtype DeregisterParams = DeregisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ Maybe PubKey
  }

getCommitteeCandidateValidator ∷ SidechainParams → Contract Validator
getCommitteeCandidateValidator sp = do
  mkValidatorWithParams rawCommitteeCandidateValidator [ toData sp ]

data StakeOwnership
  = -- | Ada stake based configuration comprises the SPO public key and signature
    AdaBasedStaking PubKey Signature
  | -- | Token based staking configuration
    TokenBasedStaking

derive instance Generic StakeOwnership _

derive instance Eq StakeOwnership

instance Show StakeOwnership where
  show = genericShow

instance ToData StakeOwnership where
  toData (AdaBasedStaking pk sig) =
    Constr (BigNum.fromInt 0) [ toData pk, toData sig ]
  toData TokenBasedStaking =
    Constr (BigNum.fromInt 1) []

instance FromData StakeOwnership where
  fromData plutusData = case plutusData of
    Constr n args
      | n == BigNum.fromInt 0 → case args of
          [ x1, x2 ] → do
            x1' ← fromData x1
            x2' ← fromData x2
            pure $ AdaBasedStaking x1' x2'
          _ → Nothing

      | n == BigNum.fromInt 1 → pure TokenBasedStaking
    _ → Nothing

newtype BlockProducerRegistration = BlockProducerRegistration
  { stakeOwnership ∷ StakeOwnership -- Verification keys required by the stake ownership model
  , sidechainPubKey ∷ ByteArray -- public key in the sidechain's desired format
  , sidechainSignature ∷ ByteArray -- Signature of the sidechain candidate
  , inputUtxo ∷ TransactionInput -- A UTxO that must be spent by the transaction
  , ownPkh ∷ PaymentPubKeyHash -- Owner public key hash
  }

derive instance Generic BlockProducerRegistration _

derive instance Newtype BlockProducerRegistration _

derive newtype instance Eq BlockProducerRegistration

instance Show BlockProducerRegistration where
  show = genericShow

instance ToData BlockProducerRegistration where
  toData
    ( BlockProducerRegistration
        { stakeOwnership
        , sidechainPubKey
        , sidechainSignature
        , inputUtxo
        , ownPkh
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData stakeOwnership
      , toData sidechainPubKey
      , toData sidechainSignature
      , toData inputUtxo
      , toData ownPkh
      ]

instance FromData BlockProducerRegistration where
  fromData plutusData = case plutusData of
    Constr n [ x1, x2, x3, x4, x5 ] → do
      guard (n == BigNum.fromInt 0)
      x1' ← fromData x1
      x2' ← fromData x2
      x3' ← fromData x3
      x4' ← fromData x4
      x5' ← fromData x5
      pure
        ( BlockProducerRegistration
            { stakeOwnership: x1'
            , sidechainPubKey: x2'
            , sidechainSignature: x3'
            , inputUtxo: x4'
            , ownPkh: x5'
            }
        )
    _ → Nothing

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams ∷ SidechainParams
  , bprmSidechainPubKey ∷ ByteArray
  , bprmInputUtxo ∷ TransactionInput -- A UTxO that must be spent by the transaction
  }

derive instance Eq BlockProducerRegistrationMsg

derive instance Generic BlockProducerRegistrationMsg _

instance Show BlockProducerRegistrationMsg where
  show = genericShow

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

instance FromData BlockProducerRegistrationMsg where
  fromData = case _ of
    Constr ix [ sp, spk, iu ] → do
      guard (BigNum.fromInt 0 == ix)
      bprmSidechainParams ← fromData sp
      bprmSidechainPubKey ← fromData spk
      bprmInputUtxo ← fromData iu
      pure $ BlockProducerRegistrationMsg
        { bprmSidechainParams
        , bprmSidechainPubKey
        , bprmInputUtxo
        }
    _ → Nothing

register ∷ RegisterParams → Contract TransactionHash
register
  ( RegisterParams
      { sidechainParams
      , stakeOwnership
      , sidechainPubKey
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

  ownRegistrations ← findOwnRegistrations ownPkh (getSPOPubKey stakeOwnership)
    valUtxos

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
      { stakeOwnership
      , sidechainPubKey
      , sidechainSignature: sidechainSig
      , inputUtxo: inputUtxo
      , ownPkh
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

  balanceSignAndSubmit "Registers Committee Candidate" { lookups, constraints }

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

  balanceSignAndSubmit "Deregister Committee Candidate" { lookups, constraints }

-- | Based on the wallet public key hash and the SPO public key, it finds the
-- | the registration UTxOs of the committee member/candidate
findOwnRegistrations ∷
  PaymentPubKeyHash →
  Maybe PubKey →
  UtxoMap →
  Contract (Array TransactionInput)
findOwnRegistrations ownPkh spoPubKey validatorUtxos = do
  mayTxIns ← Map.toUnfoldable validatorUtxos #
    parTraverse
      \(input /\ TransactionOutputWithRefScript { output: TransactionOutput out }) →
        pure do
          Datum d ← outputDatumDatum out.datum
          BlockProducerRegistration r ← fromData d
          guard
            ( (getSPOPubKey r.stakeOwnership == spoPubKey) &&
                (r.ownPkh == ownPkh)
            )
          pure input
  pure $ catMaybes mayTxIns

-- | Return SPO public key if StakeOwnership is ada based staking. Otherwise, it returns Nothing.
getSPOPubKey ∷ StakeOwnership → Maybe PubKey
getSPOPubKey (AdaBasedStaking pk _) = Just pk
getSPOPubKey _ = Nothing
