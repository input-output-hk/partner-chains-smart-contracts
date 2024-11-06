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

import Cardano.Types.BigInt as BigInt
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusData (unit)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value as Value
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
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap)
import Control.Alternative (guard)
import Data.Array (catMaybes)
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Error
  ( OffchainError(InvalidCLIParams, NotFoundInputUtxo)
  )
import TrustlessSidechain.Types (PubKey, Signature)
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  , toAddress
  )
import TrustlessSidechain.Utils.Data
  ( VersionedGenericDatum(VersionedGenericDatum)
  )
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CommitteeCandidateValidator)
  )
import Type.Row (type (+))

newtype RegisterParams = RegisterParams
  { genesisUtxo :: TransactionInput
  , stakeOwnership :: StakeOwnership
  , sidechainPubKey :: ByteArray
  , sidechainSig :: ByteArray
  , inputUtxo :: TransactionInput
  , auraKey :: ByteArray
  , grandpaKey :: ByteArray
  }

newtype DeregisterParams = DeregisterParams
  { genesisUtxo :: TransactionInput
  , spoPubKey :: Maybe PubKey
  }

getCommitteeCandidateValidator ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + r) PlutusScript.PlutusScript
getCommitteeCandidateValidator gu = do
  mkValidatorWithParams CommitteeCandidateValidator [ toData gu ]

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
      | n == BigNum.fromInt 0 -> case args of
          [ x1, x2 ] -> do
            x1' <- fromData x1
            x2' <- fromData x2
            pure $ AdaBasedStaking x1' x2'
          _ -> Nothing

      | n == BigNum.fromInt 1 -> pure TokenBasedStaking
    _ -> Nothing

newtype BlockProducerRegistration = BlockProducerRegistration
  { stakeOwnership :: StakeOwnership -- Verification keys required by the stake ownership model
  , sidechainPubKey :: ByteArray -- public key in the sidechain's desired format
  , sidechainSignature :: ByteArray -- Signature of the sidechain candidate
  , inputUtxo :: TransactionInput -- A UTxO that must be spent by the transaction
  , auraKey :: ByteArray -- sidechain authority discovery key
  , grandpaKey :: ByteArray -- sidechain grandpa key
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
        , auraKey
        , grandpaKey
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData stakeOwnership
      , toData sidechainPubKey
      , toData sidechainSignature
      , toData inputUtxo
      , toData auraKey
      , toData grandpaKey
      ]

instance FromData BlockProducerRegistration where
  fromData plutusData = case plutusData of
    Constr n [ x1, x2, x3, x4, x5, x6 ] -> do
      guard (n == BigNum.fromInt 0)
      x1' <- fromData x1
      x2' <- fromData x2
      x3' <- fromData x3
      x4' <- fromData x4
      x5' <- fromData x5
      x6' <- fromData x6
      pure
        ( BlockProducerRegistration
            { stakeOwnership: x1'
            , sidechainPubKey: x2'
            , sidechainSignature: x3'
            , inputUtxo: x4'
            , auraKey: x5'
            , grandpaKey: x6'
            }
        )
    _ -> Nothing

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmGenesisUtxo :: TransactionInput
  , bprmSidechainPubKey :: ByteArray
  , bprmInputUtxo :: TransactionInput -- A UTxO that must be spent by the transaction
  }

derive instance Eq BlockProducerRegistrationMsg

derive instance Generic BlockProducerRegistrationMsg _

instance Show BlockProducerRegistrationMsg where
  show = genericShow

instance ToData BlockProducerRegistrationMsg where
  toData
    ( BlockProducerRegistrationMsg
        { bprmGenesisUtxo
        , bprmSidechainPubKey
        , bprmInputUtxo
        }
    ) = Constr (BigNum.fromInt 0)
    [ toData bprmGenesisUtxo
    , toData bprmSidechainPubKey
    , toData bprmInputUtxo
    ]

instance FromData BlockProducerRegistrationMsg where
  fromData = case _ of
    Constr ix [ gu, spk, iu ] -> do
      guard (BigNum.fromInt 0 == ix)
      bprmGenesisUtxo <- fromData gu
      bprmSidechainPubKey <- fromData spk
      bprmInputUtxo <- fromData iu
      pure $ BlockProducerRegistrationMsg
        { bprmGenesisUtxo
        , bprmSidechainPubKey
        , bprmInputUtxo
        }
    _ -> Nothing

register ::
  forall r.
  RegisterParams ->
  Run (APP + r) TransactionHash
register
  ( RegisterParams
      { genesisUtxo
      , stakeOwnership
      , sidechainPubKey
      , sidechainSig
      , inputUtxo
      , auraKey
      , grandpaKey
      }
  ) = do
  ownPkh <- getOwnPaymentPubKeyHash
  ownAddr <- getOwnWalletAddress

  validator <- getCommitteeCandidateValidator genesisUtxo
  let valHash = PlutusScript.hash validator
  valAddr <- toAddress valHash

  ownUtxos <- Effect.utxosAt ownAddr
  valUtxos <- Effect.utxosAt valAddr

  { ownRegistrationUtxos, ownRegistrationDatums } <- findOwnRegistrations ownPkh
    (getSPOPubKey stakeOwnership)
    valUtxos

  let
    blockProducerDatum = BlockProducerRegistration
      { stakeOwnership
      , sidechainPubKey
      , sidechainSignature: sidechainSig
      , inputUtxo: inputUtxo
      , auraKey
      , grandpaKey
      }

    datum = toData $ VersionedGenericDatum
      { datum: ownPkh
      , builtinData: toData blockProducerDatum
      , version: BigInt.fromInt 0
      }

    matchingKeys (BlockProducerRegistration r1) (BlockProducerRegistration r2) =
      r1.stakeOwnership == r2.stakeOwnership
        && r1.sidechainPubKey
        == r2.sidechainPubKey
        && r1.auraKey
        == r2.auraKey
        && r1.grandpaKey
        == r2.grandpaKey

  when (any (matchingKeys blockProducerDatum) ownRegistrationDatums) $
    throw
      ( InvalidCLIParams
          "BlockProducer with given set of keys is already registered"
      )

  let
    lookups :: Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs ownUtxos
      <> Lookups.validator validator
      <> Lookups.unspentOutputs valUtxos

    constraints :: Constraints.TxConstraints
    constraints =
      -- Sending new registration to validator address
      Constraints.mustSpendPubKeyOutput inputUtxo
        <> Constraints.mustPayToScript valHash (toData datum)
          Constraints.DatumInline
          (Value.lovelaceValueOf (BigNum.fromInt 1))

        -- Consuming old registration UTxOs
        <> Constraints.mustBeSignedBy ownPkh
        <> mconcat
          ( flip Constraints.mustSpendScriptOutput (RedeemerDatum unit) <$>
              ownRegistrationUtxos
          )

  balanceSignAndSubmit "Register Committee Candidate" { lookups, constraints }

deregister ::
  forall r.
  DeregisterParams ->
  Run (APP + r) TransactionHash
deregister (DeregisterParams { genesisUtxo, spoPubKey }) = do
  ownPkh <- getOwnPaymentPubKeyHash
  ownAddr <- getOwnWalletAddress
  validator <- getCommitteeCandidateValidator genesisUtxo
  valAddr <- toAddress (PlutusScript.hash validator)
  ownUtxos <- Effect.utxosAt ownAddr
  valUtxos <- Effect.utxosAt valAddr

  { ownRegistrationUtxos } <- findOwnRegistrations ownPkh spoPubKey valUtxos

  when (null ownRegistrationUtxos)
    $ throw
        (NotFoundInputUtxo "Couldn't find registration UTxO")

  let
    lookups :: Lookups.ScriptLookups
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs ownUtxos
      <> Lookups.unspentOutputs valUtxos

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustBeSignedBy ownPkh
      <> mconcat
        ( flip Constraints.mustSpendScriptOutput (RedeemerDatum unit) <$>
            ownRegistrationUtxos
        )

  balanceSignAndSubmit "Deregister Committee Candidate" { lookups, constraints }

-- | Based on the wallet public key hash and the SPO public key, it finds the
-- | the registration UTxOs of the committee member/candidate
findOwnRegistrations ::
  forall r.
  PaymentPubKeyHash ->
  Maybe PubKey ->
  UtxoMap ->
  Run r
    { ownRegistrationUtxos :: Array TransactionInput
    , ownRegistrationDatums :: Array BlockProducerRegistration
    }
findOwnRegistrations ownPkh spoPubKey validatorUtxos = do
  mayTxInsAndBlockProducerRegistrations <- Map.toUnfoldable validatorUtxos #
    traverse
      \(input /\ TransactionOutput out) ->
        pure do
          d <- outputDatumDatum =<< out.datum
          VersionedGenericDatum { builtinData, datum: pkh } ::
            VersionedGenericDatum PaymentPubKeyHash <-
            fromData d
          BlockProducerRegistration r <- fromData builtinData
          guard
            ( (getSPOPubKey r.stakeOwnership == spoPubKey) &&
                (pkh == ownPkh)
            )
          pure (input /\ BlockProducerRegistration r)

  let
    txInsAndBlockProducerRegistrations = catMaybes
      mayTxInsAndBlockProducerRegistrations
    ownRegistrationUtxos = map fst txInsAndBlockProducerRegistrations
    ownRegistrationDatums = map snd txInsAndBlockProducerRegistrations
  pure $ { ownRegistrationUtxos, ownRegistrationDatums }

-- | Return SPO public key if StakeOwnership is ada based staking. Otherwise, it returns Nothing.
getSPOPubKey :: StakeOwnership -> Maybe PubKey
getSPOPubKey (AdaBasedStaking pk _) = Just pk
getSPOPubKey _ = Nothing
