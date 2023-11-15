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
  ( rawDParameterMintingPolicy
  , rawDParameterValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig) as Versioning

-- | Get the DummyMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDParameterMintingPolicy ∷ SidechainParams → Contract MintingPolicy
decodeDParameterMintingPolicy sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkMintingPolicyWithParams rawDParameterMintingPolicy $
    [ toData sidechainParams, toData versionOracleConfig ]

decodeDParameterValidator ∷ SidechainParams → Contract Validator
decodeDParameterValidator sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkValidatorWithParams rawDParameterValidator
    [ toData sidechainParams, toData versionOracleConfig ]

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
