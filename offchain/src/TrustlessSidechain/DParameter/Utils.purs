module TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  , getDParameterValidatorAndAddress
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , getNetworkId
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData
  ( toData
  )
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , validatorHash
  )
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import TrustlessSidechain.RawScripts
  ( rawDParameterPolicy
  , rawDParameterValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDParameterMintingPolicy ∷ SidechainParams → Contract MintingPolicy
decodeDParameterMintingPolicy sidechainParams = do
  { dParameterValidatorAddress } ← getDParameterValidatorAndAddress
    sidechainParams
  mkMintingPolicyWithParams rawDParameterPolicy $
    [ toData sidechainParams, toData dParameterValidatorAddress ]

decodeDParameterValidator ∷ SidechainParams → Contract Validator
decodeDParameterValidator sidechainParams = do
  mkValidatorWithParams rawDParameterValidator
    [ toData sidechainParams ]

getDParameterValidatorAndAddress ∷
  SidechainParams →
  Contract
    { dParameterValidator ∷ Validator
    , dParameterValidatorAddress ∷ Address
    }
getDParameterValidatorAndAddress sidechainParams = do
  dParameterValidator ← decodeDParameterValidator sidechainParams
  netId ← getNetworkId
  dParameterValidatorAddress ←
    liftContractM "cannot get d parameter validator address"
      (validatorHashEnterpriseAddress netId (validatorHash dParameterValidator))

  pure { dParameterValidator, dParameterValidatorAddress }

getDParameterMintingPolicyAndCurrencySymbol ∷
  SidechainParams →
  Contract
    { dParameterMintingPolicy ∷ MintingPolicy
    , dParameterCurrencySymbol ∷ CurrencySymbol
    }
getDParameterMintingPolicyAndCurrencySymbol sidechainParams = do
  dParameterMintingPolicy ← decodeDParameterMintingPolicy sidechainParams
  dParameterCurrencySymbol ← liftContractM
    "Failed to get DParameterMintingPolicy"
    (Value.scriptCurrencySymbol dParameterMintingPolicy)
  pure { dParameterMintingPolicy, dParameterCurrencySymbol }
