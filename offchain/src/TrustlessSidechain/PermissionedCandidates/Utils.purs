module TrustlessSidechain.PermissionedCandidates.Utils
  ( getPermissionedCandidatesMintingPolicyAndCurrencySymbol
  , getPermissionedCandidatesValidatorAndAddress
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  )
import Contract.Monad (Contract)
import Contract.PlutusData
  ( toData
  )
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , validatorHash
  )
import Contract.Value (CurrencySymbol)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getCurrencySymbol, toAddress)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( PermissionedCandidatesValidator
      , PermissionedCandidatesPolicy
      )
  )

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ∷
  SidechainParams → Contract MintingPolicy
decodePermissionedCandidatesMintingPolicy sidechainParams = do
  { permissionedCandidatesValidatorAddress } ←
    getPermissionedCandidatesValidatorAndAddress sidechainParams
  mkMintingPolicyWithParams PermissionedCandidatesPolicy
    [ toData sidechainParams, toData permissionedCandidatesValidatorAddress ]

decodePermissionedCandidatesValidator ∷ SidechainParams → Contract Validator
decodePermissionedCandidatesValidator sidechainParams = do
  mkValidatorWithParams PermissionedCandidatesValidator
    [ toData sidechainParams ]

getPermissionedCandidatesValidatorAndAddress ∷
  SidechainParams →
  Contract
    { permissionedCandidatesValidator ∷ Validator
    , permissionedCandidatesValidatorAddress ∷ Address
    }
getPermissionedCandidatesValidatorAndAddress sidechainParams = do
  permissionedCandidatesValidator ← decodePermissionedCandidatesValidator
    sidechainParams
  permissionedCandidatesValidatorAddress ←
    toAddress (validatorHash permissionedCandidatesValidator)

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
  permissionedCandidatesCurrencySymbol ←
    getCurrencySymbol PermissionedCandidatesPolicy
      permissionedCandidatesMintingPolicy
  pure
    { permissionedCandidatesMintingPolicy, permissionedCandidatesCurrencySymbol }
