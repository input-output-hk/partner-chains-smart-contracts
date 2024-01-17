-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  , SidechainAddressesExtra
  , getSidechainAddresses
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Scripts
  ( Validator
  , validatorHash
  )
import Contract.Transaction (TransactionInput)
import Data.Array as Array
import Data.Functor (map)
import Data.Map as Map
import Data.TraversableWithIndex (traverseWithIndex)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1, ATMSPlainSchnorrSecp256k1)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.DParameter.Utils as DParameter
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELProxyPolicy (getFuelProxyMintingPolicy)
import TrustlessSidechain.PermissionedCandidates.Utils as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address
  ( currencySymbolToHex
  , getCurrencySymbolHex
  , getValidatorHash
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
      )
  )
import TrustlessSidechain.Versioning.Utils
  ( getVersionOraclePolicy
  , versionOracleValidator
  )

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
  { mCandidatePermissionTokenUtxo ∷ Maybe TransactionInput
  , version ∷ Int
  }

-- | `SidechainAddressesEndpointParams` is the offchain endpoint parameter for
-- | bundling the required data to grab all the sidechain addresses.
newtype SidechainAddressesEndpointParams = SidechainAddressesEndpointParams
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  , -- Used to optionally grab the minting policy of candidate permission
    -- token.
    mCandidatePermissionTokenUtxo ∷ Maybe TransactionInput
  , version ∷ Int
  }

-- | `getSidechainAddresses` returns a `SidechainAddresses` corresponding to
-- | the given `SidechainAddressesEndpointParams` which contains related
-- | addresses and currency symbols. Moreover, it returns the currency symbol
-- | of the candidate permission token provided the `permissionTokenUtxo` is
-- | given.
getSidechainAddresses ∷
  SidechainAddressesEndpointParams → Contract SidechainAddresses
getSidechainAddresses
  ( SidechainAddressesEndpointParams
      { sidechainParams: scParams
      , atmsKind
      , mCandidatePermissionTokenUtxo
      , version
      }
  ) = do

  -- Minting policies
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap scParams).thresholdNumerator
        , thresholdDenominator: (unwrap scParams).thresholdDenominator
        }

  ds ← DistributedSet.getDs (unwrap scParams).genesisUtxo

  dsConfPolicy ← DistributedSet.dsConfPolicy
    (wrap (unwrap scParams).genesisUtxo)
  dsConfPolicyId ← getCurrencySymbolHex DsConfPolicy dsConfPolicy

  mCandidatePermissionPolicyId ← case mCandidatePermissionTokenUtxo of
    Nothing → pure Nothing
    Just permissionTokenUtxo → do
      { candidatePermissionPolicy } ←
        CandidatePermissionToken.getCandidatePermissionMintingPolicy
          $ CandidatePermissionMint
              { sidechainParams: scParams
              , candidatePermissionTokenUtxo: permissionTokenUtxo
              }
      candidatePermissionPolicyId ← getCurrencySymbolHex
        CandidatePermissionPolicy
        candidatePermissionPolicy
      pure $ Just candidatePermissionPolicyId

  { currencySymbol: checkpointCurrencySymbol } ← do
    Checkpoint.checkpointCurrencyInfo scParams
  let checkpointPolicyId = currencySymbolToHex checkpointCurrencySymbol

  { versionOracleCurrencySymbol } ← getVersionOraclePolicy scParams
  let versionOraclePolicyId = currencySymbolToHex versionOracleCurrencySymbol

  { fuelProxyCurrencySymbol } ← getFuelProxyMintingPolicy scParams
  let fuelProxyPolicyId = currencySymbolToHex fuelProxyCurrencySymbol

  { permissionedCandidatesCurrencySymbol } ←
    PermissionedCandidates.getPermissionedCandidatesMintingPolicyAndCurrencySymbol
      scParams
  let
    permissionedCandidatesPolicyId =
      currencySymbolToHex permissionedCandidatesCurrencySymbol

  { dParameterCurrencySymbol } ←
    DParameter.getDParameterMintingPolicyAndCurrencySymbol
      scParams
  let dParameterPolicyId = currencySymbolToHex dParameterCurrencySymbol

  -- Validators
  committeeCandidateValidator ←
    CommitteeCandidateValidator.getCommitteeCandidateValidator scParams

  dsInsertValidator ← DistributedSet.insertValidator ds
  dsConfValidator ← DistributedSet.dsConfValidator ds

  veresionOracleValidator ←
    versionOracleValidator scParams versionOracleCurrencySymbol

  { versionedPolicies, versionedValidators } ←
    Versioning.getVersionedPoliciesAndValidators
      { sidechainParams: scParams, atmsKind }
      version
  versionedCurrencySymbols ← Map.toUnfoldable <$> traverseWithIndex
    getCurrencySymbolHex
    versionedPolicies

  { currencySymbol: committeePlainEcdsaSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.committeePlainEcdsaSecp256k1ATMSCurrencyInfo
      { committeeCertificateMint, sidechainParams: scParams }
  let
    committeePlainEcdsaSecp256k1ATMSCurrencyInfoId = currencySymbolToHex
      committeePlainEcdsaSecp256k1ATMSCurrencySymbol

  { currencySymbol: committeePlainSchnorrSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainSchnorrSecp256k1ATMSPolicy.committeePlainSchnorrSecp256k1ATMSCurrencyInfo
      { committeeCertificateMint, sidechainParams: scParams }
  let
    committeePlainSchnorrSecp256k1ATMSCurrencyInfoId = currencySymbolToHex
      committeePlainSchnorrSecp256k1ATMSCurrencySymbol

  { permissionedCandidatesValidator } ←
    PermissionedCandidates.getPermissionedCandidatesValidatorAndAddress
      scParams

  { dParameterValidator } ←
    DParameter.getDParameterValidatorAndAddress scParams

  let
    mintingPolicies =
      [ DsConfPolicy /\ dsConfPolicyId
      , CheckpointPolicy /\ checkpointPolicyId
      , FUELProxyPolicy /\ fuelProxyPolicyId
      , VersionOraclePolicy /\ versionOraclePolicyId
      , PermissionedCandidatesPolicy /\ permissionedCandidatesPolicyId
      , DParameterPolicy /\ dParameterPolicyId
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
      , VersionOracleValidator /\ veresionOracleValidator
      , PermissionedCandidatesValidator /\ permissionedCandidatesValidator
      , DParameterValidator /\ dParameterValidator
      ] <> Map.toUnfoldable versionedValidators

  addresses ← traverse (traverse getAddr) validators
  let validatorHashes = map (map getValidatorHash) validators

  pure
    { addresses
    , mintingPolicies
    , validatorHashes
    }

-- | Print the bech32 serialised address of a given validator
getAddr ∷ Validator → Contract String
getAddr v = do
  netId ← Address.getNetworkId
  addr ← Monad.liftContractM ("Cannot get validator address") $
    Address.validatorHashEnterpriseAddress
      netId
      (validatorHash v)
  serialised ← Address.addressToBech32 addr
  pure serialised
