-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesExtra
  , getSidechainAddresses
  , currencySymbolToHex
  ) where

import Contract.Prelude

import Contract.Address as Address
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.CandidatePermissionToken (CandidatePermissionMint(..))
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELProxyPolicy (getFuelProxyMintingPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (assetClass)
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.UpdateCommitteeHash.Types (UpdateCommitteeHash(..))
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( updateCommitteeHashValidator
  )
import TrustlessSidechain.Utils.Logging (class Display)
import TrustlessSidechain.Utils.Logging as Utils.Logging
import TrustlessSidechain.Versioning as Versioning
import TrustlessSidechain.Versioning.Types (ScriptId(..), VersionOracle(..))
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
  { addresses ∷ Array (Tuple ScriptId String)
  , mintingPolicies ∷ Array (Tuple ScriptId String)
  }

-- | `SidechainAddressesExtra` provides extra information for creating more
-- | addresses related to the sidechain.
-- | In particular, this allows us to optionally grab the minting policy of the
-- | candidate permission token.
type SidechainAddressesExtra =
  { mCandidatePermissionTokenUtxo ∷ Maybe TransactionInput
  , version ∷ Int
  }

-- | `getSidechainAddresses` returns a `SidechainAddresses` corresponding to
-- | the given `SidechainParams` which contains related addresses and currency
-- | symbols. Moreover, it returns the currency symbol of the candidate
-- | permission token provided the `permissionTokenUtxo` is given.
getSidechainAddresses ∷
  SidechainParams → SidechainAddressesExtra → Contract SidechainAddresses
getSidechainAddresses scParams { mCandidatePermissionTokenUtxo, version } = do
  { committeeHashCurrencySymbol, committeeHashTokenName } ←
    UpdateCommitteeHash.getCommitteeHashPolicy scParams

  ds ← DistributedSet.getDs (unwrap scParams).genesisUtxo

  dsConfPolicy ← DistributedSet.dsConfPolicy
    (wrap (unwrap scParams).genesisUtxo)
  dsConfPolicyId ← getCurrencySymbolHex dsConfPolicy

  mCandidatePermissionPolicyId ← case mCandidatePermissionTokenUtxo of
    Nothing → pure Nothing
    Just permissionTokenUtxo → do
      { candidatePermissionPolicy } ←
        CandidatePermissionToken.getCandidatePermissionMintingPolicy
          $ CandidatePermissionMint
              { sidechainParams: scParams
              , candidatePermissionTokenUtxo: permissionTokenUtxo
              }
      candidatePermissionPolicyId ← getCurrencySymbolHex candidatePermissionPolicy
      pure $ Just candidatePermissionPolicyId

  { versionOracleCurrencySymbol } ← getVersionOraclePolicy scParams
  let versionOraclePolicyId = currencySymbolToHex versionOracleCurrencySymbol

  { fuelProxyCurrencySymbol } ← getFuelProxyMintingPolicy scParams
  let fuelProxyPolicyId = currencySymbolToHex fuelProxyCurrencySymbol

  -- Validators
  committeeCandidateValidatorAddr ← do
    validator ← CommitteeCandidateValidator.getCommitteeCandidateValidator
      scParams
    getAddr validator

  let
    updateCommittHashParams = assetClass committeeHashCurrencySymbol
      committeeHashTokenName

  merkleRootTokenCurrencySymbol ←
    getVersionedCurrencySymbol scParams $ VersionOracle
      { version: BigInt.fromInt version, scriptId: MerkleRootTokenPolicy }

  committeeHashValidatorAddr ←
    do
      let
        uch = UpdateCommitteeHash
          { sidechainParams: scParams
          , uchAssetClass: updateCommittHashParams
          , merkleRootTokenCurrencySymbol
          }
      validator ← updateCommitteeHashValidator uch
      getAddr validator

  dsInsertValidatorAddr ← do
    validator ← DistributedSet.insertValidator ds
    getAddr validator
  dsConfValidatorAddr ← do
    validator ← DistributedSet.dsConfValidator ds
    getAddr validator

  veresionOracleValidatorAddr ← do
    validator ← versionOracleValidator scParams versionOracleCurrencySymbol
    getAddr validator

  { versionedPolicies, versionedValidators } ←
    Versioning.getVersionedPoliciesAndValidators scParams version
  versionedCurrencySymbols ← Map.toUnfoldable <$> traverse getCurrencySymbolHex
    versionedPolicies
  versionedAddresses ← Map.toUnfoldable <$> traverse getAddr versionedValidators

  let
    mintingPolicies =
      [ DSConfPolicy /\ dsConfPolicyId
      , VersionOraclePolicy /\ versionOraclePolicyId
      , FUELProxyPolicy /\ fuelProxyPolicyId
      ]
        <>
          Array.catMaybes
            [ map (CandidatePermissionPolicy /\ _)
                mCandidatePermissionPolicyId
            ]
        <> versionedCurrencySymbols

    addresses =
      [ CommitteeCandidateValidator /\ committeeCandidateValidatorAddr
      , CommitteeHashValidator /\ committeeHashValidatorAddr
      , DSConfValidator /\ dsConfValidatorAddr
      , DSInsertValidator /\ dsInsertValidatorAddr
      , VersionOracleValidator /\ veresionOracleValidatorAddr
      ] <> versionedAddresses
  pure
    { addresses
    , mintingPolicies
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
getCurrencySymbolHex ∷ MintingPolicy → Contract String
getCurrencySymbolHex mp = do
  let msg = report "getCurrencySymbolHex"
  cs ← Monad.liftContractM (msg "Cannot get currency symbol") $
    Value.scriptCurrencySymbol mp
  pure $ currencySymbolToHex cs

-- | Convert a currency symbol to hex encoded string
currencySymbolToHex ∷ CurrencySymbol → String
currencySymbolToHex =
  ByteArray.byteArrayToHex <<< Value.getCurrencySymbol

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "GetSidechainAddresses", fun: _ }
