module TrustlessSidechain.MinotaurStake.Utils
  ( getMinotaurStakeMintingPolicyAndCurrencySymbol
  , getMinotaurStakeValidatorAndAddress
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  )
import Contract.PlutusData
  ( toData
  )
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , validatorHash
  )
import Contract.Value (CurrencySymbol)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Address
  ( getCurrencySymbol
  , toAddress
  )
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( MinotaurStakeValidator
      , MinotaurStakePolicy
      )
  )
import Type.Row (type (+))

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeMinotaurStakePolicy ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r) MintingPolicy
decodeMinotaurStakePolicy = do
  { minotaurStakeValidatorAddress } ← getMinotaurStakeValidatorAndAddress
  mkMintingPolicyWithParams MinotaurStakePolicy
    [ toData minotaurStakeValidatorAddress ]

decodeMinotaurStakeValidator ∷
  ∀ r.
  Run (EXCEPT OffchainError + r) Validator
decodeMinotaurStakeValidator = do
  mkValidatorWithParams MinotaurStakeValidator
    []

getMinotaurStakeValidatorAndAddress ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r)
    { minotaurStakeValidator ∷ Validator
    , minotaurStakeValidatorAddress ∷ Address
    }
getMinotaurStakeValidatorAndAddress = do
  minotaurStakeValidator ← decodeMinotaurStakeValidator
  minotaurStakeValidatorAddress ←
    toAddress (validatorHash minotaurStakeValidator)

  pure
    { minotaurStakeValidator, minotaurStakeValidatorAddress }

getMinotaurStakeMintingPolicyAndCurrencySymbol ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r)
    { minotaurStakeMintingPolicy ∷ MintingPolicy
    , minotaurStakeCurrencySymbol ∷ CurrencySymbol
    }
getMinotaurStakeMintingPolicyAndCurrencySymbol = do
  minotaurStakeMintingPolicy ← decodeMinotaurStakePolicy
  minotaurStakeCurrencySymbol ←
    getCurrencySymbol MinotaurStakePolicy
      minotaurStakeMintingPolicy
  pure
    { minotaurStakeMintingPolicy, minotaurStakeCurrencySymbol }
