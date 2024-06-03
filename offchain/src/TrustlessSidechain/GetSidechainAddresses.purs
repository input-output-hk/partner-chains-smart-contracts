-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  , SidechainAddressesExtra
  , getSidechainAddresses
  ) where

import Contract.Prelude

import Cardano.Types.Address
  ( toBech32
  )
import Cardano.AsCbor (encodeCbor)
import Data.ByteArray (byteArrayToHex)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Data.Array as Array
import Data.Functor (map)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds
      ( ATMSPlainEcdsaSecp256k1
      , ATMSPlainSchnorrSecp256k1
      )
  , CommitteeCertificateMint
      ( CommitteeCertificateMint
      )
  )
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.DParameter.Utils as DParameter
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.FUELProxyPolicy (getFuelProxyMintingPolicy)
import TrustlessSidechain.InitSidechain.Utils as InitSidechain
import TrustlessSidechain.PermissionedCandidates.Utils as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( toAddress
  )
import TrustlessSidechain.Versioning as Versioning
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( DsConfPolicy
      , CandidatePermissionPolicy
      , CheckpointPolicy
      , FUELProxyPolicy
      , VersionOraclePolicy
      , PermissionedCandidatesPolicy
      , DParameterPolicy
      , CommitteePlainEcdsaSecp256k1ATMSPolicy
      , CommitteePlainSchnorrSecp256k1ATMSPolicy
      , CommitteeCandidateValidator
      , DsConfValidator
      , DsInsertValidator
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
    addresses ∷ Array (Tuple ScriptId String)
  , -- currency symbols
    mintingPolicies ∷ Array (Tuple ScriptId String)
  , -- Hex encoded validator hashes
    validatorHashes ∷ Array (Tuple ScriptId String)
  }

-- | `SidechainAddressesExtra` provides extra information for creating more
-- | addresses related to the sidechain.
-- | In particular, this allows us to optionally grab the minting policy of the
-- | candidate permission token.
type SidechainAddressesExtra =
  { usePermissionToken ∷ Boolean
  , version ∷ Int
  }

-- | `SidechainAddressesEndpointParams` is the offchain endpoint parameter for
-- | bundling the required data to grab all the sidechain addresses.
newtype SidechainAddressesEndpointParams = SidechainAddressesEndpointParams
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  , -- Used to optionally grab the minting policy of candidate permission
    -- token.
    usePermissionToken ∷ Boolean
  , version ∷ Int
  }

-- | `getSidechainAddresses` returns a `SidechainAddresses` corresponding to
-- | the given `SidechainAddressesEndpointParams` which contains related
-- | addresses and currency symbols. Moreover, it returns the currency symbol
-- | of the candidate permission token provided the `permissionTokenUtxo` is
-- | given.
getSidechainAddresses ∷
  ∀ r.
  SidechainAddressesEndpointParams →
  Run (EXCEPT OffchainError + WALLET + r) SidechainAddresses
getSidechainAddresses
  ( SidechainAddressesEndpointParams
      { sidechainParams
      , atmsKind
      , usePermissionToken
      , version
      }
  ) = do

  -- Minting policies
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator: (unwrap sidechainParams).thresholdDenominator
        }

  ds ← DistributedSet.getDs sidechainParams

  { mintingPolicy: dsConfPolicy } ← DistributedSet.dsConfCurrencyInfo
    sidechainParams
  let dsConfPolicyId = byteArrayToHex $ unwrap $ encodeCbor $ PlutusScript.hash dsConfPolicy

  mCandidatePermissionPolicyId ←
    if usePermissionToken then do
      { mintingPolicy: candidatePermissionPolicy } ←
        CandidatePermissionToken.candidatePermissionCurrencyInfo sidechainParams
      let candidatePermissionPolicyId = byteArrayToHex $ unwrap $ encodeCbor $ PlutusScript.hash candidatePermissionPolicy
      pure $ Just candidatePermissionPolicyId
    else pure Nothing

  { currencySymbol: checkpointCurrencySymbol } ← do
    Checkpoint.checkpointCurrencyInfo sidechainParams
  let checkpointPolicyId = byteArrayToHex $ unwrap $ encodeCbor checkpointCurrencySymbol

  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sidechainParams
  let versionOraclePolicyId = byteArrayToHex $ unwrap $ encodeCbor versionOracleCurrencySymbol

  { fuelProxyCurrencySymbol } ← getFuelProxyMintingPolicy sidechainParams
  let fuelProxyPolicyId = byteArrayToHex $ unwrap $ encodeCbor fuelProxyCurrencySymbol

  { permissionedCandidatesCurrencySymbol } ←
    PermissionedCandidates.getPermissionedCandidatesMintingPolicyAndCurrencySymbol
      sidechainParams
  let
    permissionedCandidatesPolicyId =
      byteArrayToHex $ unwrap $ encodeCbor permissionedCandidatesCurrencySymbol

  { dParameterCurrencySymbol } ←
    DParameter.getDParameterMintingPolicyAndCurrencySymbol
      sidechainParams
  let dParameterPolicyId = byteArrayToHex $ unwrap $ encodeCbor dParameterCurrencySymbol

  { currencySymbol: initTokenCurrencySymbol } ←
    InitSidechain.initTokenCurrencyInfo sidechainParams
  let initTokenPolicyId = byteArrayToHex $ unwrap $ encodeCbor initTokenCurrencySymbol

  -- Validators
  committeeCandidateValidator ←
    CommitteeCandidateValidator.getCommitteeCandidateValidator sidechainParams

  dsInsertValidator ← DistributedSet.insertValidator ds
  dsConfValidator ← DistributedSet.dsConfValidator ds

  versionOracleValidator ←
    versionOracleValidator sidechainParams

  { versionedPolicies, versionedValidators } ←
    Versioning.getExpectedVersionedPoliciesAndValidators
      { sidechainParams: sidechainParams, atmsKind }
      version
  let versionedCurrencySymbols = Array.fromFoldable $ map
        ( \(Tuple scriptId mps) → (Tuple scriptId (byteArrayToHex $ unwrap $ encodeCbor mps))
        )
        versionedPolicies

  { currencySymbol: committeePlainEcdsaSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.committeePlainEcdsaSecp256k1ATMSCurrencyInfo
      { committeeCertificateMint, sidechainParams }
  let
    committeePlainEcdsaSecp256k1ATMSCurrencyInfoId = byteArrayToHex $ unwrap $ encodeCbor
      committeePlainEcdsaSecp256k1ATMSCurrencySymbol

  { currencySymbol: committeePlainSchnorrSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainSchnorrSecp256k1ATMSPolicy.committeePlainSchnorrSecp256k1ATMSCurrencyInfo
      { committeeCertificateMint, sidechainParams }
  let
    committeePlainSchnorrSecp256k1ATMSCurrencyInfoId = byteArrayToHex $ unwrap $ encodeCbor
      committeePlainSchnorrSecp256k1ATMSCurrencySymbol

  { permissionedCandidatesValidator } ←
    PermissionedCandidates.getPermissionedCandidatesValidatorAndAddress
      sidechainParams

  { dParameterValidator } ←
    DParameter.getDParameterValidatorAndAddress sidechainParams

  let
    mintingPolicies :: Array (Tuple ScriptId String)
    mintingPolicies =
      [ DsConfPolicy /\ dsConfPolicyId
      , CheckpointPolicy /\ checkpointPolicyId
      , FUELProxyPolicy /\ fuelProxyPolicyId
      , VersionOraclePolicy /\ versionOraclePolicyId
      , PermissionedCandidatesPolicy /\ permissionedCandidatesPolicyId
      , DParameterPolicy /\ dParameterPolicyId
      , InitTokenPolicy /\ initTokenPolicyId
      ]
        <>
          Array.catMaybes
            [ map (CandidatePermissionPolicy /\ _)
                mCandidatePermissionPolicyId
            ]
        <> versionedCurrencySymbols
        <>
          ( case atmsKind of
              ATMSPlainEcdsaSecp256k1 →
                [ CommitteePlainEcdsaSecp256k1ATMSPolicy
                    /\ committeePlainEcdsaSecp256k1ATMSCurrencyInfoId
                ]
              ATMSPlainSchnorrSecp256k1 →
                [ CommitteePlainSchnorrSecp256k1ATMSPolicy
                    /\ committeePlainSchnorrSecp256k1ATMSCurrencyInfoId
                ]
              _ → []
          )

    validators =
      [ CommitteeCandidateValidator /\ committeeCandidateValidator
      , DsConfValidator /\ dsConfValidator
      , DsInsertValidator /\ dsInsertValidator
      , VersionOracleValidator /\ versionOracleValidator
      , PermissionedCandidatesValidator /\ permissionedCandidatesValidator
      , DParameterValidator /\ dParameterValidator
      ] <> List.toUnfoldable versionedValidators

  addresses ← traverse (traverse getAddr) validators
  let validatorHashes = map (map (byteArrayToHex <<< unwrap <<< encodeCbor <<< PlutusScript.hash)) validators

  pure
    { addresses
    , mintingPolicies
    , validatorHashes
    }

-- | Print the bech32 serialised address of a given validator
getAddr ∷ ∀ r. PlutusScript → Run (EXCEPT OffchainError + WALLET + r) String
getAddr v = do
  addr ← toAddress (PlutusScript.hash v)
  pure $ toBech32 addr
