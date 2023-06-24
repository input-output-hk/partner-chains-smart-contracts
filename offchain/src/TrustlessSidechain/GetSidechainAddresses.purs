-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesExtra
  , getSidechainAddresses
  , currencySymbolToHex
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Address as Address
import Contract.CborBytes as CborBytes
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Array as Array
import TrustlessSidechain.CandidatePermissionToken (CandidatePermissionMint(..))
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSAggregateSignatures(Plain)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELMintingPolicy as FUELMintingPolicy
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.MerkleRoot.Utils (merkleRootTokenValidator)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types (UpdateCommitteeHash(..))
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.Utils.Logging as Utils.Logging

-- | `SidechainAddresses` is an record of `Array`s which uniquely associates a `String`
-- | identifier with a hex encoded validator address / currency symbol of a
-- | sidechain validator / minting policy.
-- |
-- | See `getSidechainAddresses` for more details.
type SidechainAddresses =
  { -- bech32 addresses
    addresses ∷ Array (Tuple String String)
  , --  currency symbols
    mintingPolicies ∷ Array (Tuple String String)
  , -- cbor of the Plutus Address type.
    plutusDataAddresses ∷ Array (Tuple String String)
  }

-- | `SidechainAddressesExtra` provides extra information for creating more
-- | addresses related to the sidechain.
-- | In particular, this allows us to optionally grab the minting policy of the
-- | candidate permission token.
type SidechainAddressesExtra =
  { mCandidatePermissionTokenUtxo ∷ Maybe TransactionInput }

-- | `getSidechainAddresses` returns a `SidechainAddresses` corresponding to
-- | the given `SidechainParams` which contains related addresses and currency
-- | symbols. Moreover, it returns the currency symbol of the candidate
-- | permission token provided the `permissionTokenUtxo` is given.
getSidechainAddresses ∷
  SidechainParams → SidechainAddressesExtra → Contract SidechainAddresses
getSidechainAddresses scParams { mCandidatePermissionTokenUtxo } = do
  -- Minting policies
  { fuelMintingPolicyCurrencySymbol } ← FUELMintingPolicy.getFuelMintingPolicy
    scParams
  let fuelMintingPolicyId = currencySymbolToHex fuelMintingPolicyCurrencySymbol

  { merkleRootTokenCurrencySymbol } ←
    MerkleRoot.getMerkleRootTokenMintingPolicy scParams
  let
    merkleRootTokenMintingPolicyId = currencySymbolToHex
      merkleRootTokenCurrencySymbol

  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy scParams
  let committeeNftPolicyId = currencySymbolToHex committeeOracleCurrencySymbol

  ds ← DistributedSet.getDs (unwrap scParams).genesisUtxo
  { dsKeyPolicyCurrencySymbol } ← DistributedSet.getDsKeyPolicy ds

  let dsKeyPolicyPolicyId = currencySymbolToHex dsKeyPolicyCurrencySymbol

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

  -- Validators
  committeeCandidateValidatorAddr ← do
    validator ← CommitteeCandidateValidator.getCommitteeCandidateValidator
      scParams
    getAddr validator
  merkleRootTokenValidatorAddr ← do
    validator ← merkleRootTokenValidator scParams
    getAddr validator

  { committeeHashValidatorAddr, committeeHashValidatorAddrPlutus } ←
    do
      -- TODO: this is going to get all replaced soon?
      let
        committeeCertificateMint =
          CommitteeCertificateMint
            { thresholdNumerator: (unwrap scParams).thresholdNumerator
            , thresholdDenominator: (unwrap scParams).thresholdDenominator
            , committeeOraclePolicy: committeeOracleCurrencySymbol
            }
      { committeeCertificateVerificationCurrencySymbol } ←
        CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
          committeeCertificateMint
          (Plain mempty)
      let
        uch = UpdateCommitteeHash
          { sidechainParams: scParams
          , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
          , merkleRootTokenCurrencySymbol
          , committeeCertificateVerificationCurrencySymbol
          }
      { validator, address } ← getUpdateCommitteeHashValidator uch
      bech32Addr ← getAddr validator

      pure
        { committeeHashValidatorAddr: bech32Addr
        , committeeHashValidatorAddrPlutus: getPlutusAddr address
        }

  dsInsertValidatorAddr ← do
    validator ← DistributedSet.insertValidator ds
    getAddr validator
  dsConfValidatorAddr ← do
    validator ← DistributedSet.dsConfValidator ds
    getAddr validator

  let
    mintingPolicies =
      [ "FuelMintingPolicyId" /\ fuelMintingPolicyId
      , "MerkleRootTokenMintingPolicyId" /\ merkleRootTokenMintingPolicyId
      , "CommitteeNftPolicyId" /\ committeeNftPolicyId
      , "DSKeyPolicy" /\ dsKeyPolicyPolicyId
      , "DSConfPolicy" /\ dsConfPolicyId
      ]
        <>
          Array.catMaybes
            [ map ("CandidatePermissionTokenPolicy" /\ _)
                mCandidatePermissionPolicyId
            ]

    addresses =
      [ "CommitteeCandidateValidator" /\ committeeCandidateValidatorAddr
      , "MerkleRootTokenValidator" /\ merkleRootTokenValidatorAddr
      , "CommitteeHashValidator" /\ committeeHashValidatorAddr
      , "DSConfValidator" /\ dsConfValidatorAddr
      , "DSInsertValidator" /\ dsInsertValidatorAddr
      ]

    plutusDataAddresses =
      [ "CommitteeHashValidator" /\ committeeHashValidatorAddrPlutus
      ]

  pure
    { addresses
    , mintingPolicies
    , plutusDataAddresses
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

-- | Gets the hex encoded string of the cbor representation of an Address
getPlutusAddr ∷ Address → String
getPlutusAddr =
  ByteArray.byteArrayToHex
    <<< CborBytes.cborBytesToByteArray
    <<< PlutusData.serializeData

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
report ∷ String → String → String
report = Utils.Logging.mkReport "GetSidechainAddresses"
