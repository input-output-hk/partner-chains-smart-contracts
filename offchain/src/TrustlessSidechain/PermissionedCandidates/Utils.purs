module TrustlessSidechain.PermissionedCandidates.Utils
  ( getPermissionedCandidatesMintingPolicyAndCurrencySymbol
  , getPermissionedCandidatesValidatorAndAddress
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
      ( PermissionedCandidatesValidator
      , PermissionedCandidatesPolicy
      )
  )
import Type.Row (type (+))

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) MintingPolicy
decodePermissionedCandidatesMintingPolicy sidechainParams = do
  { permissionedCandidatesValidatorAddress } ←
    getPermissionedCandidatesValidatorAndAddress sidechainParams
  mkMintingPolicyWithParams PermissionedCandidatesPolicy
    [ toData sidechainParams, toData permissionedCandidatesValidatorAddress ]

decodePermissionedCandidatesValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) Validator
decodePermissionedCandidatesValidator sidechainParams = do
  mkValidatorWithParams PermissionedCandidatesValidator
    [ toData sidechainParams ]

getPermissionedCandidatesValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
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
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
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
