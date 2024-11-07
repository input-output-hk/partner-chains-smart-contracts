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
import Contract.Transaction (TransactionInput)
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(InvalidAddress))
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

-- | Get the OnlyMintMintingPolicy by applying `genesisUtxo` to the dummy
-- | minting policy.
decodePermissionedCandidatesMintingPolicy ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r) PlutusScript
decodePermissionedCandidatesMintingPolicy genesisUtxo = do
  { permissionedCandidatesValidatorAddress } <-
    getPermissionedCandidatesValidatorAndAddress genesisUtxo
  plutusAddress <-
    Run.note
      ( InvalidAddress "Invalid permissioned candidates validator address."
          permissionedCandidatesValidatorAddress
      )
      $ fromCardano permissionedCandidatesValidatorAddress
  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  mkMintingPolicyWithParams PermissionedCandidatesPolicy
    [ toData genesisUtxo, toData versionOracleConfig, toData plutusAddress ]

decodePermissionedCandidatesValidator ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodePermissionedCandidatesValidator genesisUtxo = do
  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  mkValidatorWithParams PermissionedCandidatesValidator
    [ toData genesisUtxo, toData versionOracleConfig ]

getPermissionedCandidatesValidatorAndAddress ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { permissionedCandidatesValidator :: PlutusScript
    , permissionedCandidatesValidatorAddress :: Address
    }
getPermissionedCandidatesValidatorAndAddress genesisUtxo = do
  permissionedCandidatesValidator <- decodePermissionedCandidatesValidator
    genesisUtxo
  permissionedCandidatesValidatorAddress <-
    toAddress (PlutusScript.hash permissionedCandidatesValidator)

  pure
    { permissionedCandidatesValidator, permissionedCandidatesValidatorAddress }

getPermissionedCandidatesMintingPolicyAndCurrencySymbol ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { permissionedCandidatesMintingPolicy :: PlutusScript
    , permissionedCandidatesCurrencySymbol :: ScriptHash
    }
getPermissionedCandidatesMintingPolicyAndCurrencySymbol genesisUtxo = do
  permissionedCandidatesMintingPolicy <-
    decodePermissionedCandidatesMintingPolicy
      genesisUtxo
  let
    permissionedCandidatesCurrencySymbol = PlutusScript.hash
      permissionedCandidatesMintingPolicy
  pure
    { permissionedCandidatesMintingPolicy, permissionedCandidatesCurrencySymbol }
