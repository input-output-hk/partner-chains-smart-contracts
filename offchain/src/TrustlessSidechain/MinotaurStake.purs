module TrustlessSidechain.MinotaurStake
  ( mkMinotaurDelegateLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
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
