module TrustlessSidechain.MinotaurStake
  ( mkMinotaurDelegateLookupsAndConstraints
  , getOwnMinotaurDelegations
  , getMinotaurDelegationsForGivenStakePoolId
  , mkMinotaurCancelDelegationLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction
  ( OutputDatum(OutputDatum)
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
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
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
  )
import TrustlessSidechain.MinotaurStake.Types
  ( MinotaurStakeDatum(MinotaurStakeDatum)
  , MinotaurStakePolicyRedeemer(MintMinotaurStake)
  )
import TrustlessSidechain.MinotaurStake.Utils as MinotaurStake
import TrustlessSidechain.Utils.Address as Utils
import Type.Row (type (+))

minotaurStakeTokenName ∷ TokenName
minotaurStakeTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Minotaur Stake"

-- | Lookups and constraints for minting/burning exactly
-- | one Minotaur Stake token for use in the CLI endpoints
-- | for delegating or canceling a delegation.
mkMinotaurStakeMintBurnLookupsAndConstraints ∷
  ∀ r.
  { partnerChainRewardAddress ∷ ByteArray
  , stakePoolId ∷ ByteArray
  } →
  -- | `True` if minting (delegating), otherwise `False`.
  Boolean →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkMinotaurStakeMintBurnLookupsAndConstraints
  { partnerChainRewardAddress, stakePoolId }
  isMint = do
  { minotaurStakeMintingPolicy, minotaurStakeCurrencySymbol } ←
    MinotaurStake.getMinotaurStakeMintingPolicyAndCurrencySymbol

  { minotaurStakeValidatorAddress } ←
    MinotaurStake.getMinotaurStakeValidatorAndAddress

  stakePubKeyHash ← Utils.getOwnStakePubKeyHash
  minotaurStakeValidatorHash ← Utils.toValidatorHash
    minotaurStakeValidatorAddress
  let
    quantity = if isMint then one else -one

    value ∷ Value
    value = Value.singleton
      minotaurStakeCurrencySymbol
      minotaurStakeTokenName
      quantity

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

mkMinotaurDelegateLookupsAndConstraints ∷
  ∀ r.
  { partnerChainRewardAddress ∷ ByteArray
  , stakePoolId ∷ ByteArray
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkMinotaurDelegateLookupsAndConstraints = flip
  mkMinotaurStakeMintBurnLookupsAndConstraints
  true

mkMinotaurCancelDelegationLookupsAndConstraints ∷
  ∀ r.
  { partnerChainRewardAddress ∷ ByteArray
  , stakePoolId ∷ ByteArray
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkMinotaurCancelDelegationLookupsAndConstraints = flip
  mkMinotaurStakeMintBurnLookupsAndConstraints
  false
