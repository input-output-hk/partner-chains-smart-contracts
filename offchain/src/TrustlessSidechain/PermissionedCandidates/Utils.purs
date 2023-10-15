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
import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData
  ( toData
  )
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  , validatorHash
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import TrustlessSidechain.RawScripts
  ( rawPermissionedCandidatesMintingPolicy
  , rawPermissionedCandidatesValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig) as Versioning

-- | Get the DummyMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ∷
  SidechainParams → Contract MintingPolicy
decodePermissionedCandidatesMintingPolicy sidechainParams = do
  let
    script = decodeTextEnvelope rawPermissionedCandidatesMintingPolicy >>=
      plutusScriptV2FromEnvelope
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ Scripts.applyArgs unapplied
    [ toData sidechainParams, toData versionOracleConfig ]
  pure $ PlutusMintingPolicy applied

decodePermissionedCandidatesValidator ∷ SidechainParams → Contract Validator
decodePermissionedCandidatesValidator sidechainParams = do
  let
    script = decodeTextEnvelope rawPermissionedCandidatesValidator >>=
      plutusScriptV2FromEnvelope
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ Scripts.applyArgs unapplied
    [ toData sidechainParams, toData versionOracleConfig ]
  pure $ Validator applied

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
