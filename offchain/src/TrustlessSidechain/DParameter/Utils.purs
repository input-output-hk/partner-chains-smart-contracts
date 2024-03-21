module TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  , getDParameterValidatorAndAddress
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
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getCurrencySymbol, toAddress)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( DParameterValidator
      , DParameterPolicy
      )
  )
import Type.Row (type (+))

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDParameterMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) MintingPolicy
decodeDParameterMintingPolicy sidechainParams = do
  { dParameterValidatorAddress } ← getDParameterValidatorAndAddress
    sidechainParams
  mkMintingPolicyWithParams DParameterPolicy $
    [ toData sidechainParams, toData dParameterValidatorAddress ]

decodeDParameterValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) Validator
decodeDParameterValidator sidechainParams = do
  mkValidatorWithParams DParameterValidator [ toData sidechainParams ]

getDParameterValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { dParameterValidator ∷ Validator
    , dParameterValidatorAddress ∷ Address
    }
getDParameterValidatorAndAddress sidechainParams = do
  dParameterValidator ← decodeDParameterValidator
    sidechainParams
  dParameterValidatorAddress ←
    toAddress (validatorHash dParameterValidator)

  pure { dParameterValidator, dParameterValidatorAddress }

getDParameterMintingPolicyAndCurrencySymbol ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { dParameterMintingPolicy ∷ MintingPolicy
    , dParameterCurrencySymbol ∷ CurrencySymbol
    }
getDParameterMintingPolicyAndCurrencySymbol sidechainParams = do
  dParameterMintingPolicy ← decodeDParameterMintingPolicy sidechainParams
  dParameterCurrencySymbol ←
    getCurrencySymbol DParameterPolicy
      dParameterMintingPolicy
  pure { dParameterMintingPolicy, dParameterCurrencySymbol }
