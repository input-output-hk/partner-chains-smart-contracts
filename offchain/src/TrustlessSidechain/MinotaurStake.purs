module TrustlessSidechain.MinotaurStake
  ( mkMinotaurDelegateLookupsAndConstraints
  , getOwnMinotaurDelegations
  , getMinotaurDelegationsForGivenStakePoolId
  , mkMinotaurCancelDelegationLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Address (StakePubKeyHash)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), fromData, toData)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction
  ( OutputDatum(OutputDatum)
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Control.Alternative (guard)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Filterable (filterMap)
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, GenericInternalError)
  )
import TrustlessSidechain.MinotaurStake.Types
  ( MinotaurStakeDatum(MinotaurStakeDatum)
  , MinotaurStakePolicyRedeemer(MintMinotaurStake, BurnMinotaurStake)
  )
import TrustlessSidechain.MinotaurStake.Utils as MinotaurStake
import TrustlessSidechain.Utils.Address as Utils
import Type.Row (type (+))

minotaurStakeTokenName ∷ TokenName
minotaurStakeTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Minotaur Stake"

mkMinotaurDelegateLookupsAndConstraints ∷
  ∀ r.
  { partnerChainRewardAddress ∷ ByteArray
  , stakePoolId ∷ ByteArray
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkMinotaurDelegateLookupsAndConstraints
  { partnerChainRewardAddress, stakePoolId } = do
  { minotaurStakeMintingPolicy, minotaurStakeCurrencySymbol } ←
    MinotaurStake.getMinotaurStakeMintingPolicyAndCurrencySymbol

  { minotaurStakeValidatorAddress } ←
    MinotaurStake.getMinotaurStakeValidatorAndAddress

  stakePubKeyHash ← Utils.getOwnStakePubKeyHash
  minotaurStakeValidatorHash ← Utils.toValidatorHash
    minotaurStakeValidatorAddress
  let
    value ∷ Value
    value = Value.singleton
      minotaurStakeCurrencySymbol
      minotaurStakeTokenName
      one

    minotaurStakeDatum ∷ Datum
    minotaurStakeDatum = Datum $ toData $
      MinotaurStakeDatum
        { partnerChainRewardAddress
        , stakePoolId
        , stakeCurrencySymbol: minotaurStakeCurrencySymbol
        , stakePubKeyHash: unwrap stakePubKeyHash
        }

    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy minotaurStakeMintingPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemer
        (Scripts.mintingPolicyHash minotaurStakeMintingPolicy)
        (Redeemer $ toData MintMinotaurStake)
        minotaurStakeTokenName
        one
        <> Constraints.mustPayToScript
          minotaurStakeValidatorHash
          minotaurStakeDatum
          DatumInline
          value
        <> Constraints.mustBeSignedBy (wrap $ unwrap stakePubKeyHash)

  pure { lookups, constraints }

getOwnMinotaurDelegations ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Array MinotaurStakeDatum)
getOwnMinotaurDelegations = do
  ownStakePubKeyHash ← Utils.getOwnStakePubKeyHash
  { minotaurStakeCurrencySymbol } ←
    MinotaurStake.getMinotaurStakeMintingPolicyAndCurrencySymbol

  { minotaurStakeValidatorAddress } ←
    MinotaurStake.getMinotaurStakeValidatorAndAddress

  allMinotaurUtxos ← utxosAt minotaurStakeValidatorAddress

  let
    minotaurUtxos =
      Array.fromFoldable
        $ Map.values
        $ filterMap
            ( \output → case output of
                ( TransactionOutputWithRefScript
                    { output:
                        (TransactionOutput { amount, datum: OutputDatum (Datum d) })
                    }
                ) → do
                  guard
                    ( Value.valueOf amount minotaurStakeCurrencySymbol
                        minotaurStakeTokenName > BigInt.fromInt 0
                    )
                  minoDatum@(MinotaurStakeDatum { stakePubKeyHash }) ← fromData d
                  guard (stakePubKeyHash == unwrap ownStakePubKeyHash)
                  pure minoDatum
                _ → Nothing
            )
            allMinotaurUtxos

  pure minotaurUtxos

getMinotaurDelegationsForGivenStakePoolId ∷
  ∀ r.
  { stakePoolId ∷ ByteArray } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Array MinotaurStakeDatum)
getMinotaurDelegationsForGivenStakePoolId { stakePoolId } = do
  { minotaurStakeCurrencySymbol } ←
    MinotaurStake.getMinotaurStakeMintingPolicyAndCurrencySymbol

  { minotaurStakeValidatorAddress } ←
    MinotaurStake.getMinotaurStakeValidatorAndAddress

  allMinotaurUtxos ← utxosAt minotaurStakeValidatorAddress

  let
    minotaurUtxos =
      Array.fromFoldable
        $ Map.values
        $ filterMap
            ( \output → case output of
                ( TransactionOutputWithRefScript
                    { output:
                        (TransactionOutput { amount, datum: OutputDatum (Datum d) })
                    }
                ) → do
                  guard
                    ( Value.valueOf amount minotaurStakeCurrencySymbol
                        minotaurStakeTokenName > BigInt.fromInt 0
                    )
                  minoDatum@(MinotaurStakeDatum { stakePoolId: spId }) ← fromData d
                  guard (spId == stakePoolId)
                  pure minoDatum
                _ → Nothing
            )
            allMinotaurUtxos

  pure minotaurUtxos

mkMinotaurCancelDelegationLookupsAndConstraints ∷
  ∀ r.
  { partnerChainRewardAddress ∷ ByteArray
  , stakePoolId ∷ ByteArray
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkMinotaurCancelDelegationLookupsAndConstraints
  { partnerChainRewardAddress
  , stakePoolId
  } = do

  stakePubKeyHash ← Utils.getOwnStakePubKeyHash

  { minotaurStakeValidatorTxIn
  , minotaurStakeValidatorTxOut
  , minotaurStakeValidator
  } ← getMinotaurDelegatedTokenTxIn stakePubKeyHash
    { partnerChainRewardAddress, stakePoolId }

  let
    lookups ∷ ScriptLookups Void
    lookups = Lookups.validator minotaurStakeValidator
      <> Lookups.unspentOutputs
        (Map.singleton minotaurStakeValidatorTxIn minotaurStakeValidatorTxOut)

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustSpendScriptOutput
        minotaurStakeValidatorTxIn
        (Redeemer $ toData BurnMinotaurStake)
        <> Constraints.mustBeSignedBy (wrap $ unwrap stakePubKeyHash)

  pure { lookups, constraints }

-- | Internal. Finds the UTxO at the validator address
-- | associated with a delegation of native Partner Chain
-- | tokens of this wallet, returning the TxIn and related
-- | fields needed to construct lookups and constraints.
getMinotaurDelegatedTokenTxIn ∷
  ∀ r.
  StakePubKeyHash →
  { partnerChainRewardAddress ∷ ByteArray
  , stakePoolId ∷ ByteArray
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { minotaurStakeValidatorTxIn ∷ TransactionInput
    , minotaurStakeValidatorTxOut ∷ TransactionOutputWithRefScript
    , minotaurStakeValidatorHash ∷ Scripts.ValidatorHash
    , minotaurStakeValidator ∷ Scripts.Validator
    , minotaurStakeMintingPolicy ∷ Scripts.MintingPolicy
    , minotaurStakeCurrencySymbol ∷ Value.CurrencySymbol
    }
getMinotaurDelegatedTokenTxIn
  stakePubKeyHash
  { partnerChainRewardAddress
  , stakePoolId
  } = do
  { minotaurStakeMintingPolicy, minotaurStakeCurrencySymbol } ←
    MinotaurStake.getMinotaurStakeMintingPolicyAndCurrencySymbol

  { minotaurStakeValidatorAddress, minotaurStakeValidator } ←
    MinotaurStake.getMinotaurStakeValidatorAndAddress

  minotaurStakeValidatorHash ← Utils.toValidatorHash
    minotaurStakeValidatorAddress

  validatorUtxos ← utxosAt minotaurStakeValidatorAddress

  let
    isDelegatedToken
      ( TransactionOutputWithRefScript
          { output: (TransactionOutput { amount, datum: OutputDatum (Datum d) })
          }
      ) =
      let
        amountCheck =
          Value.valueOf amount minotaurStakeCurrencySymbol minotaurStakeTokenName
            > BigInt.fromInt 0
        datumCheck =
          maybe false
            ( \( MinotaurStakeDatum
                   { stakePubKeyHash: k
                   , partnerChainRewardAddress: ra
                   , stakePoolId: spid
                   }
               ) → k
                == unwrap stakePubKeyHash
                && ra
                == partnerChainRewardAddress
                && spid
                == stakePoolId
            )
            (fromData d)
      in
        amountCheck && datumCheck
    isDelegatedToken _ = false

    delegatedUTxOs = Map.toUnfoldable $ Map.filter isDelegatedToken
      validatorUtxos

  (minotaurStakeValidatorTxIn /\ minotaurStakeValidatorTxOut) ←
    case delegatedUTxOs of
      [ x ] → pure x
      [] → throw $ NotFoundUtxo
        "UTxO with Minotaur delegated stake not found at validator address"
      _ → throw $ GenericInternalError
        """More than one UTxO with Minotaur delegated stake found
         at validator address for given stakePubKeyHash, reward address and
         stake pool id. Should never happen."""

  pure
    { minotaurStakeValidatorTxIn
    , minotaurStakeValidatorTxOut
    , minotaurStakeValidatorHash
    , minotaurStakeValidator
    , minotaurStakeMintingPolicy
    , minotaurStakeCurrencySymbol
    }
