module TrustlessSidechain.PermissionedCandidates.Utils
  ( getPermissionedCandidatesMintingPolicyAndCurrencySymbol
  , getPermissionedCandidatesValidatorAndAddress
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
  ( rawPermissionedCandidatesMintingPolicy
  , rawPermissionedCandidatesValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig) as Versioning

-- | Get the DummyMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ∷
  SidechainParams → Contract MintingPolicy
decodePermissionedCandidatesMintingPolicy sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkMintingPolicyWithParams rawPermissionedCandidatesMintingPolicy
    [ toData sidechainParams, toData versionOracleConfig ]

decodePermissionedCandidatesValidator ∷ SidechainParams → Contract Validator
decodePermissionedCandidatesValidator sidechainParams = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  mkValidatorWithParams rawPermissionedCandidatesValidator
    [ toData sidechainParams, toData versionOracleConfig ]

getPermissionedCandidatesValidatorAndAddress ∷
  SidechainParams →
  Contract
    { permissionedCandidatesValidator ∷ Validator
    , permissionedCandidatesValidatorAddress ∷ Address
    }
getPermissionedCandidatesValidatorAndAddress sidechainParams = do
  permissionedCandidatesValidator ← decodePermissionedCandidatesValidator
    sidechainParams
  netId ← getNetworkId
  permissionedCandidatesValidatorAddress ←
    liftContractM "cannot get d parameter validator address"
      ( validatorHashEnterpriseAddress netId
          (validatorHash permissionedCandidatesValidator)
      )

  pure
    { permissionedCandidatesValidator, permissionedCandidatesValidatorAddress }

getPermissionedCandidatesMintingPolicyAndCurrencySymbol ∷
  SidechainParams →
  Contract
    { permissionedCandidatesMintingPolicy ∷ MintingPolicy
    , permissionedCandidatesCurrencySymbol ∷ CurrencySymbol
    }
getPermissionedCandidatesMintingPolicyAndCurrencySymbol sidechainParams = do
  permissionedCandidatesMintingPolicy ← decodePermissionedCandidatesMintingPolicy
    sidechainParams
  permissionedCandidatesCurrencySymbol ← liftContractM
    "Failed to get PermissionedCandidatesMintingPolicy"
    (Value.scriptCurrencySymbol permissionedCandidatesMintingPolicy)
  pure
    { permissionedCandidatesMintingPolicy, permissionedCandidatesCurrencySymbol }
