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

import Contract.Prelude hiding (unit)

import Contract.Address
  ( PaymentPubKeyHash
  )
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , RedeemerDatum(RedeemerDatum)
  , fromData
  , toData
  )
import Cardano.Types.PlutusData (unit)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Transaction
  ( TransactionHash
  )

import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.OutputDatum (outputDatumDatum)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap)
import Cardano.Types.Value as Value
import Control.Alternative (guard)
import Data.Array (catMaybes)
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Error
  ( OffchainError(InvalidCLIParams, NotFoundInputUtxo)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (PubKey, Signature)
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  , toAddress
  )
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CommitteeCandidateValidator)
  )
import Type.Row (type (+))
import Partial.Unsafe (unsafePartial)

newtype RegisterParams = RegisterParams
  { sidechainParams ∷ SidechainParams
  , stakeOwnership ∷ StakeOwnership
  , sidechainPubKey ∷ ByteArray
  , sidechainSig ∷ ByteArray
  , inputUtxo ∷ TransactionInput
  , usePermissionToken ∷ Boolean
  , auraKey ∷ ByteArray
  , grandpaKey ∷ ByteArray
  , spoTokenInfo ∷ ByteArray -- contains partnerchain spo_fixed_fee and spo_poll_margin
  }

newtype DeregisterParams = DeregisterParams
  { sidechainParams ∷ SidechainParams
  , spoPubKey ∷ Maybe PubKey
  }

getCommitteeCandidateValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) PlutusScript.PlutusScript
getCommitteeCandidateValidator sp = do
  mkValidatorWithParams CommitteeCandidateValidator [ toData sp ]

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
  , auraKey ∷ ByteArray -- sidechain authority discovery key
  , grandpaKey ∷ ByteArray -- sidechain grandpa key
  , spoTokenInfo ∷ ByteArray -- contains partnerchain spo_fixed_fee and spo_poll_margin
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
        , auraKey
        , grandpaKey
        , spoTokenInfo
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData stakeOwnership
      , toData sidechainPubKey
      , toData sidechainSignature
      , toData inputUtxo
      , toData ownPkh
      , toData auraKey
      , toData grandpaKey
      , toData spoTokenInfo
      ]

instance FromData BlockProducerRegistration where
  fromData plutusData = case plutusData of
    Constr n [ x1, x2, x3, x4, x5, x6, x7, x8 ] → do
      guard (n == BigNum.fromInt 0)
      x1' ← fromData x1
      x2' ← fromData x2
      x3' ← fromData x3
      x4' ← fromData x4
      x5' ← fromData x5
      x6' ← fromData x6
      x7' ← fromData x7
      x8' ← fromData x8
      pure
        ( BlockProducerRegistration
            { stakeOwnership: x1'
            , sidechainPubKey: x2'
            , sidechainSignature: x3'
            , inputUtxo: x4'
            , ownPkh: x5'
            , auraKey: x6'
            , grandpaKey: x7'
            , spoTokenInfo: x8'
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

register ∷
  ∀ r.
  RegisterParams →
  Run (APP + r) TransactionHash
register
  ( RegisterParams
      { sidechainParams
      , stakeOwnership
      , sidechainPubKey
      , sidechainSig
      , inputUtxo
      , usePermissionToken
      , auraKey
      , grandpaKey
      , spoTokenInfo
      }
  ) = do
  ownPkh ← getOwnPaymentPubKeyHash
  ownAddr ← getOwnWalletAddress

  validator ← getCommitteeCandidateValidator sidechainParams
  let valHash = PlutusScript.hash validator
  valAddr ← toAddress valHash

  ownUtxos ← Effect.utxosAt ownAddr
  valUtxos ← Effect.utxosAt valAddr

  { ownRegistrationUtxos, ownRegistrationDatums } ← findOwnRegistrations ownPkh
    (getSPOPubKey stakeOwnership)
    valUtxos

  let
    datum = BlockProducerRegistration
      { stakeOwnership
      , sidechainPubKey
      , sidechainSignature: sidechainSig
      , inputUtxo: inputUtxo
      , ownPkh
      , auraKey
      , grandpaKey
      , spoTokenInfo
      }

    matchingKeys (BlockProducerRegistration r1) (BlockProducerRegistration r2) =
      r1.stakeOwnership == r2.stakeOwnership
        && r1.sidechainPubKey
        == r2.sidechainPubKey
        && r1.auraKey
        == r2.auraKey
        && r1.grandpaKey
        == r2.grandpaKey

  when (any (matchingKeys datum) ownRegistrationDatums) $
    throw
      ( InvalidCLIParams
          "BlockProducer with given set of keys is already registered"
      )

  { currencySymbol: candidateCurrencySymbol
  , mintingPolicy: candidateMintingPolicy
  } ← CandidatePermissionToken.candidatePermissionCurrencyInfo sidechainParams

  let
    val = Value.lovelaceValueOf (BigNum.fromInt 1)
      `Value.add`
        if usePermissionToken then Value.singleton candidateCurrencySymbol
          CandidatePermissionToken.candidatePermissionTokenName
          (BigNum.fromInt 1)
        else unsafePartial mempty

    lookups ∷ Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs ownUtxos
      <> Lookups.validator validator
      <> Lookups.unspentOutputs valUtxos
      <>
        if usePermissionToken then Lookups.plutusMintingPolicy candidateMintingPolicy
        else mempty

    constraints ∷ Constraints.TxConstraints
    constraints =
      -- Sending new registration to validator address
      Constraints.mustSpendPubKeyOutput inputUtxo
        <> Constraints.mustPayToScript valHash (toData datum)
          Constraints.DatumInline
          (unsafePartial $ fromJust val)

        -- Consuming old registration UTxOs
        <> Constraints.mustBeSignedBy ownPkh
        <> mconcat
          ( flip Constraints.mustSpendScriptOutput (RedeemerDatum unit) <$>
              ownRegistrationUtxos
          )

  balanceSignAndSubmit "Register Committee Candidate" { lookups, constraints }

deregister ∷
  ∀ r.
  DeregisterParams →
  Run (APP + r) TransactionHash
deregister (DeregisterParams { sidechainParams, spoPubKey }) = do
  ownPkh ← getOwnPaymentPubKeyHash
  ownAddr ← getOwnWalletAddress
  validator ← getCommitteeCandidateValidator sidechainParams
  valAddr ← toAddress (PlutusScript.hash validator)
  ownUtxos ← Effect.utxosAt ownAddr
  valUtxos ← Effect.utxosAt valAddr

  { ownRegistrationUtxos } ← findOwnRegistrations ownPkh spoPubKey valUtxos

  when (null ownRegistrationUtxos)
    $ throw
        (NotFoundInputUtxo "Couldn't find registration UTxO")

  let
    lookups ∷ Lookups.ScriptLookups
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs ownUtxos
      <> Lookups.unspentOutputs valUtxos

    constraints ∷ Constraints.TxConstraints
    constraints = Constraints.mustBeSignedBy ownPkh
      <> mconcat
        ( flip Constraints.mustSpendScriptOutput (RedeemerDatum unit) <$>
            ownRegistrationUtxos
        )

  balanceSignAndSubmit "Deregister Committee Candidate" { lookups, constraints }

-- | Based on the wallet public key hash and the SPO public key, it finds the
-- | the registration UTxOs of the committee member/candidate
findOwnRegistrations ∷
  ∀ r.
  PaymentPubKeyHash →
  Maybe PubKey →
  UtxoMap →
  Run r
    { ownRegistrationUtxos ∷ Array TransactionInput
    , ownRegistrationDatums ∷ Array BlockProducerRegistration
    }
findOwnRegistrations ownPkh spoPubKey validatorUtxos = do
  mayTxInsAndBlockProducerRegistrations ← Map.toUnfoldable validatorUtxos #
    traverse
      \(input /\ TransactionOutput out) →
        pure do
          d ← outputDatumDatum =<< out.datum
          BlockProducerRegistration r ← fromData d
          guard
            ( (getSPOPubKey r.stakeOwnership == spoPubKey) &&
                (r.ownPkh == ownPkh)
            )
          pure (input /\ BlockProducerRegistration r)

  let
    txInsAndBlockProducerRegistrations = catMaybes
      mayTxInsAndBlockProducerRegistrations
    ownRegistrationUtxos = map fst txInsAndBlockProducerRegistrations
    ownRegistrationDatums = map snd txInsAndBlockProducerRegistrations
  pure $ { ownRegistrationUtxos, ownRegistrationDatums }

-- | Return SPO public key if StakeOwnership is ada based staking. Otherwise, it returns Nothing.
getSPOPubKey ∷ StakeOwnership → Maybe PubKey
getSPOPubKey (AdaBasedStaking pk _) = Just pk
getSPOPubKey _ = Nothing
