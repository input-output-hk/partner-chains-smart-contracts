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
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(InvalidAddress))
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toAddress)
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
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash as ScriptHash
import Type.Row (type (+))
import Cardano.Plutus.Types.Address (fromCardano)

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodePermissionedCandidatesMintingPolicy sidechainParams = do
  { permissionedCandidatesValidatorAddress } ←
    getPermissionedCandidatesValidatorAndAddress sidechainParams
  plutusAddress <- Run.note
    (InvalidAddress "Invalid permissioned candidates validator address." permissionedCandidatesValidatorAddress)
    $ fromCardano permissionedCandidatesValidatorAddress
  mkMintingPolicyWithParams PermissionedCandidatesPolicy
    [ toData sidechainParams, toData plutusAddress ]

decodePermissionedCandidatesValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) PlutusScript
decodePermissionedCandidatesValidator sidechainParams = do
  mkValidatorWithParams PermissionedCandidatesValidator
    [ toData sidechainParams ]

getPermissionedCandidatesValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { permissionedCandidatesValidator ∷ PlutusScript
    , permissionedCandidatesValidatorAddress ∷ Address
    }
getPermissionedCandidatesValidatorAndAddress sidechainParams = do
  permissionedCandidatesValidator ← decodePermissionedCandidatesValidator
    sidechainParams
  permissionedCandidatesValidatorAddress ←
    toAddress (PlutusScript.hash permissionedCandidatesValidator)

  pure
    { permissionedCandidatesValidator, permissionedCandidatesValidatorAddress }

getPermissionedCandidatesMintingPolicyAndCurrencySymbol ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { permissionedCandidatesMintingPolicy ∷ PlutusScript
    , permissionedCandidatesCurrencySymbol ∷ ScriptHash
    }
getPermissionedCandidatesMintingPolicyAndCurrencySymbol sidechainParams = do
  permissionedCandidatesMintingPolicy ← decodePermissionedCandidatesMintingPolicy
    sidechainParams
  let permissionedCandidatesCurrencySymbol = PlutusScript.hash permissionedCandidatesMintingPolicy
  pure
    { permissionedCandidatesMintingPolicy, permissionedCandidatesCurrencySymbol }
