module TrustlessSidechain.PermissionedCandidates.Utils
  ( getPermissionedCandidatesMintingPolicyAndCurrencySymbol
  , getPermissionedCandidatesValidatorAndAddress
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address (fromCardano)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Address
  ( Address
  )
import Contract.PlutusData
  ( toData
  )
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
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
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | Get the OnlyMintMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r) PlutusScript
decodePermissionedCandidatesMintingPolicy sidechainParams = do
  { permissionedCandidatesValidatorAddress } <-
    getPermissionedCandidatesValidatorAndAddress sidechainParams
  plutusAddress <-
    Run.note
      ( InvalidAddress "Invalid permissioned candidates validator address."
          permissionedCandidatesValidatorAddress
      )
      $ fromCardano permissionedCandidatesValidatorAddress
  versionOracleConfig <- Versioning.getVersionOracleConfig sidechainParams
  mkMintingPolicyWithParams PermissionedCandidatesPolicy
    [ toData sidechainParams, toData versionOracleConfig, toData plutusAddress ]

decodePermissionedCandidatesValidator ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodePermissionedCandidatesValidator sidechainParams = do
  versionOracleConfig <- Versioning.getVersionOracleConfig sidechainParams
  mkValidatorWithParams PermissionedCandidatesValidator
    [ toData sidechainParams, toData versionOracleConfig ]

getPermissionedCandidatesValidatorAndAddress ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { permissionedCandidatesValidator :: PlutusScript
    , permissionedCandidatesValidatorAddress :: Address
    }
getPermissionedCandidatesValidatorAndAddress sidechainParams = do
  permissionedCandidatesValidator <- decodePermissionedCandidatesValidator
    sidechainParams
  permissionedCandidatesValidatorAddress <-
    toAddress (PlutusScript.hash permissionedCandidatesValidator)

  pure
    { permissionedCandidatesValidator, permissionedCandidatesValidatorAddress }

getPermissionedCandidatesMintingPolicyAndCurrencySymbol ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { permissionedCandidatesMintingPolicy :: PlutusScript
    , permissionedCandidatesCurrencySymbol :: ScriptHash
    }
getPermissionedCandidatesMintingPolicyAndCurrencySymbol sidechainParams = do
  permissionedCandidatesMintingPolicy <-
    decodePermissionedCandidatesMintingPolicy
      sidechainParams
  let
    permissionedCandidatesCurrencySymbol = PlutusScript.hash
      permissionedCandidatesMintingPolicy
  pure
    { permissionedCandidatesMintingPolicy, permissionedCandidatesCurrencySymbol }
