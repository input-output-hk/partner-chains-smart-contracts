-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  , SidechainAddressesExtra
  , getSidechainAddresses
  , currencySymbolToHex
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , ValidatorHash(ValidatorHash)
  , validatorHash
  )
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Ctl.Internal.Serialization.Hash (scriptHashToBytes)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Functor (map)
import Data.Map as Map
import Data.TraversableWithIndex (traverseWithIndex)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointParameter(CheckpointParameter)
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1, ATMSPlainSchnorrSecp256k1)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy as CommitteePlainEcdsaSecp256k1ATMSPolicy
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy as CommitteePlainSchnorrSecp256k1ATMSPolicy
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELProxyPolicy (getFuelProxyMintingPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (assetClass)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript)
  , OffchainError(InternalError)
  )
import TrustlessSidechain.Versioning as Versioning
import TrustlessSidechain.Versioning.Types
  ( ScriptId(..)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils
  ( getVersionOraclePolicy
  , getVersionedCurrencySymbol
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
  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
      { committeeCertificateMint, sidechainParams: scParams }
      atmsKind

  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy scParams

  let committeeNftPolicyId = currencySymbolToHex committeeOracleCurrencySymbol

  ds ← DistributedSet.getDs (unwrap scParams).genesisUtxo

  dsConfPolicy ← DistributedSet.dsConfPolicy
    (wrap (unwrap scParams).genesisUtxo)
  dsConfPolicyId ← getCurrencySymbolHex DSConfPolicy dsConfPolicy

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

  { checkpointCurrencySymbol } ← do
    Checkpoint.getCheckpointPolicy scParams
  let checkpointPolicyId = currencySymbolToHex checkpointCurrencySymbol

  { versionOracleCurrencySymbol } ← getVersionOraclePolicy scParams
  let versionOraclePolicyId = currencySymbolToHex versionOracleCurrencySymbol

  { fuelProxyCurrencySymbol } ← getFuelProxyMintingPolicy scParams
  let fuelProxyPolicyId = currencySymbolToHex fuelProxyCurrencySymbol

  -- Validators
  committeeCandidateValidator ←
    CommitteeCandidateValidator.getCommitteeCandidateValidator scParams

  merkleRootTokenCurrencySymbol ←
    getVersionedCurrencySymbol scParams $ VersionOracle
      { version: BigInt.fromInt version, scriptId: MerkleRootTokenPolicy }

  { validator: committeeHashValidator } ←
    do
      let
        uch = UpdateCommitteeHash
          { sidechainParams: scParams
          , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
          , merkleRootTokenCurrencySymbol
          , committeeCertificateVerificationCurrencySymbol
          }
      getUpdateCommitteeHashValidator uch

  dsInsertValidator ← DistributedSet.insertValidator ds
  dsConfValidator ← DistributedSet.dsConfValidator ds

  checkpointValidator ← do
    let
      checkpointParam = CheckpointParameter
        { sidechainParams: scParams
        , checkpointAssetClass: assetClass checkpointCurrencySymbol
            Checkpoint.initCheckpointMintTn
        , committeeOracleCurrencySymbol
        , committeeCertificateVerificationCurrencySymbol
        }
    Checkpoint.checkpointValidator checkpointParam

  veresionOracleValidator ←
    versionOracleValidator scParams versionOracleCurrencySymbol

  { versionedPolicies, versionedValidators } ←
    Versioning.getVersionedPoliciesAndValidators
      { sidechainParams: scParams, atmsKind }
      version
  versionedCurrencySymbols ← Map.toUnfoldable <$> traverseWithIndex
    getCurrencySymbolHex
    versionedPolicies

  { committeePlainEcdsaSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainEcdsaSecp256k1ATMSPolicy.getCommitteePlainEcdsaSecp256k1ATMSPolicy
      { committeeCertificateMint, sidechainParams: scParams }
  let
    committeePlainEcdsaSecp256k1ATMSPolicyId = currencySymbolToHex
      committeePlainEcdsaSecp256k1ATMSCurrencySymbol

  { committeePlainSchnorrSecp256k1ATMSCurrencySymbol } ←
    CommitteePlainSchnorrSecp256k1ATMSPolicy.getCommitteePlainSchnorrSecp256k1ATMSPolicy
      { committeeCertificateMint, sidechainParams: scParams }
  let
    committeePlainSchnorrSecp256k1ATMSPolicyId = currencySymbolToHex
      committeePlainSchnorrSecp256k1ATMSCurrencySymbol

  let
    mintingPolicies =
      [ CommitteeNftPolicy /\ committeeNftPolicyId
      , DSConfPolicy /\ dsConfPolicyId
      , CheckpointPolicy /\ checkpointPolicyId
      , FUELProxyPolicy /\ fuelProxyPolicyId
      , VersionOraclePolicy /\ versionOraclePolicyId
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
                [ CommitteePlainEcdsaSecp256k1ATMSPolicyId
                    /\ committeePlainEcdsaSecp256k1ATMSPolicyId
                ]
              ATMSPlainSchnorrSecp256k1 →
                [ CommitteePlainSchnorrSecp256k1ATMSPolicyId
                    /\ committeePlainSchnorrSecp256k1ATMSPolicyId
                ]
              _ → []
          )

    validators =
      [ CommitteeCandidateValidator /\ committeeCandidateValidator
      , CommitteeHashValidator /\ committeeHashValidator
      , DSConfValidator /\ dsConfValidator
      , DSInsertValidator /\ dsInsertValidator
      , VersionOracleValidator /\ veresionOracleValidator
      , CheckpointValidator /\ checkpointValidator
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

-- | `getCurrencySymbolHex` converts a minting policy to its hex encoded
-- | currency symbol
getCurrencySymbolHex ∷ ScriptId → MintingPolicy → Contract String
getCurrencySymbolHex name mp = do
  cs ← Monad.liftContractM (show (InternalError (InvalidScript $ show name))) $
    Value.scriptCurrencySymbol mp
  pure $ currencySymbolToHex cs

-- | `getValidatorHashHex` converts a validator hash to a hex encoded string
getValidatorHashHex ∷ ValidatorHash → String
getValidatorHashHex (ValidatorHash sh) =
  ByteArray.byteArrayToHex $ unwrap $ scriptHashToBytes sh

-- | `getValidatorHash` converts a validator to a hex encoded string
getValidatorHash ∷ Validator → String
getValidatorHash = getValidatorHashHex <<< validatorHash

-- | Convert a currency symbol to hex encoded string
currencySymbolToHex ∷ CurrencySymbol → String
currencySymbolToHex =
  ByteArray.byteArrayToHex <<< Value.getCurrencySymbol
