-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  , SidechainAddressesExtra
  , getSidechainAddresses
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Address
  ( toBech32
  )
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Data.Array as Array
import Data.ByteArray (byteArrayToHex)
import Data.Functor (map)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.DParameter.Utils as DParameter
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Utils as InitSidechain
import TrustlessSidechain.PermissionedCandidates.Utils as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( toAddress
  )
import TrustlessSidechain.Utils.Asset
  ( currencySymbolToHex
  )
import TrustlessSidechain.Versioning as Versioning
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( VersionOraclePolicy
      , PermissionedCandidatesPolicy
      , DParameterPolicy
      , CommitteeCandidateValidator
      , VersionOracleValidator
      , PermissionedCandidatesValidator
      , DParameterValidator
      , InitTokenPolicy
      )
  )
import TrustlessSidechain.Versioning.Utils
  ( getVersionOraclePolicy
  , versionOracleValidator
  )
import Type.Row (type (+))

-- | `SidechainAddresses` is an record of `Array`s which uniquely associates a `String`
-- | identifier with a hex encoded validator address / currency symbol of a
-- | sidechain validator / minting policy.
-- |
-- | See `getSidechainAddresses` for more details.
type SidechainAddresses =
  { -- bech32 addresses
    addresses :: Array (Tuple ScriptId String)
  , -- currency symbols
    mintingPolicies :: Array (Tuple ScriptId String)
  , -- Hex encoded validator hashes
    validatorHashes :: Array (Tuple ScriptId String)
  }

-- | `SidechainAddressesExtra` provides extra information for creating more
-- | addresses related to the sidechain.
-- | In particular, this allows us to optionally grab the minting policy of the
-- | candidate permission token.
type SidechainAddressesExtra =
  { version :: Int
  }

-- | `SidechainAddressesEndpointParams` is the offchain endpoint parameter for
-- | bundling the required data to grab all the sidechain addresses.
newtype SidechainAddressesEndpointParams = SidechainAddressesEndpointParams
  { sidechainParams :: SidechainParams
  , version :: Int
  }

-- | `getSidechainAddresses` returns a `SidechainAddresses` corresponding to
-- | the given `SidechainAddressesEndpointParams` which contains related
-- | addresses and currency symbols. Moreover, it returns the currency symbol
-- | of the candidate permission token provided the `permissionTokenUtxo` is
-- | given.
getSidechainAddresses ::
  forall r.
  SidechainAddressesEndpointParams ->
  Run (EXCEPT OffchainError + WALLET + READER Env + r) SidechainAddresses
getSidechainAddresses
  ( SidechainAddressesEndpointParams
      { sidechainParams
      , version
      }
  ) = do

  -- Minting policies

  { versionOracleCurrencySymbol } <- getVersionOraclePolicy sidechainParams
  let
    versionOraclePolicyId = currencySymbolToHex
      versionOracleCurrencySymbol

  { permissionedCandidatesCurrencySymbol } <-
    PermissionedCandidates.getPermissionedCandidatesMintingPolicyAndCurrencySymbol
      sidechainParams
  let
    permissionedCandidatesPolicyId =
      currencySymbolToHex permissionedCandidatesCurrencySymbol

  { dParameterCurrencySymbol } <-
    DParameter.getDParameterMintingPolicyAndCurrencySymbol
      sidechainParams
  let
    dParameterPolicyId = currencySymbolToHex
      dParameterCurrencySymbol

  { currencySymbol: initTokenCurrencySymbol } <-
    InitSidechain.initTokenCurrencyInfo sidechainParams
  let
    initTokenPolicyId = currencySymbolToHex
      initTokenCurrencySymbol

  -- Validators
  committeeCandidateValidator <-
    CommitteeCandidateValidator.getCommitteeCandidateValidator sidechainParams

  versionOracleValidator <-
    versionOracleValidator sidechainParams

  { versionedPolicies, versionedValidators } <-
    Versioning.getExpectedVersionedPoliciesAndValidators
      sidechainParams
      version
  let
    versionedCurrencySymbols = Array.fromFoldable $ map
      ( \(Tuple scriptId mps) ->
          (Tuple scriptId (currencySymbolToHex $ PlutusScript.hash mps))
      )
      versionedPolicies

  { permissionedCandidatesValidator } <-
    PermissionedCandidates.getPermissionedCandidatesValidatorAndAddress
      sidechainParams

  { dParameterValidator } <-
    DParameter.getDParameterValidatorAndAddress sidechainParams

  let
    mintingPolicies :: Array (Tuple ScriptId String)
    mintingPolicies =
      [ VersionOraclePolicy /\ versionOraclePolicyId
      , PermissionedCandidatesPolicy /\ permissionedCandidatesPolicyId
      , DParameterPolicy /\ dParameterPolicyId
      , InitTokenPolicy /\ initTokenPolicyId
      ]
        <> versionedCurrencySymbols

    validators =
      [ CommitteeCandidateValidator /\ committeeCandidateValidator
      , VersionOracleValidator /\ versionOracleValidator
      , PermissionedCandidatesValidator /\ permissionedCandidatesValidator
      , DParameterValidator /\ dParameterValidator
      ] <> List.toUnfoldable versionedValidators

  addresses <- traverse (traverse getAddr) validators
  let
    validatorHashes = map
      (map (byteArrayToHex <<< unwrap <<< encodeCbor <<< PlutusScript.hash))
      validators

  pure
    { addresses
    , mintingPolicies
    , validatorHashes
    }

-- | Print the bech32 serialised address of a given validator
getAddr ::
  forall r. PlutusScript -> Run (EXCEPT OffchainError + WALLET + r) String
getAddr v = do
  addr <- toAddress (PlutusScript.hash v)
  pure $ toBech32 addr
