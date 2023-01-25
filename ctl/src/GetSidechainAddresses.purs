-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hex encoded validator and currency symbol.
module GetSidechainAddresses (SidechainAddresses, getSidechainAddresses) where

import Contract.Prelude

import CommitteCandidateValidator as CommitteCandidateValidator
import Contract.Address as Address
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import DistributedSet as DistributedSet
import FUELMintingPolicy as FUELMintingPolicy
import MPTRoot as MPTRoot
import MPTRoot.Utils (mptRootTokenValidator)
import SidechainParams (SidechainParams)
import Types (assetClass)
import UpdateCommitteeHash as UpdateCommitteeHash
import UpdateCommitteeHash.Types (UpdateCommitteeHash(..))
import UpdateCommitteeHash.Utils (updateCommitteeHashValidator)
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | `SidechainAddresses` is an record of `Array`s which uniquely associates a `String`
-- | identifier with a hex encoded validator address / currency symbol of a
-- | sidechain validator / minting policy.
-- |
-- | See `getSidechainAddresses` for more details.
type SidechainAddresses =
  { addresses ∷ Array (Tuple String String)
  , mintingPolicies ∷ Array (Tuple String String)
  }

-- | `getSidechainAddresses` returns a `SidechainAddresses` corresponding to
-- | the given `SidechainParams` which contains:
-- |    - addresses:
-- |        - the validator address of the committee candidate validator
-- |    - minting policies:
-- |        - the currency symbol of the fuel minting policy
-- |        - the currency symbol of the mpt root token minting policy
getSidechainAddresses ∷ SidechainParams → Contract () SidechainAddresses
getSidechainAddresses scParams = do
  -- Minting policies
  fuelMintingPolicy ← FUELMintingPolicy.getFuelMintingPolicy scParams
  fuelMintingPolicyId ← getCurrencySymbolHex fuelMintingPolicy

  { mptRootTokenCurrencySymbol } ←
    MPTRoot.getMptRootTokenMintingPolicy scParams
  let
    mptRootTokenMintingPolicyId = currencySymbolToHex mptRootTokenCurrencySymbol

  { committeeHashCurrencySymbol, committeeHashTokenName } ←
    UpdateCommitteeHash.getCommitteeHashPolicy scParams
  let committeeNftPolicyId = currencySymbolToHex committeeHashCurrencySymbol

  { dsKeyPolicyCurrencySymbol } ← DistributedSet.getDsKeyPolicy scParams
  let dsKeyPolicyPolicyId = currencySymbolToHex dsKeyPolicyCurrencySymbol

  dsConfPolicy ← DistributedSet.dsConfPolicy
    (wrap (unwrap scParams).genesisUtxo)
  dsConfPolicyId ← getCurrencySymbolHex dsConfPolicy

  -- Validators
  committeeCandidateValidatorAddr ← do
    validator ← CommitteCandidateValidator.getCommitteeCandidateValidator
      scParams
    getAddr validator
  mptRootTokenValidatorAddr ← do
    validator ← mptRootTokenValidator scParams
    getAddr validator

  let
    updateCommittHashParams = assetClass committeeHashCurrencySymbol
      committeeHashTokenName
  committeeHashValidatorAddr ←
    do
      let
        uch = UpdateCommitteeHash
          { sidechainParams: scParams
          , uchAssetClass: updateCommittHashParams
          , mptRootTokenCurrencySymbol
          }
      validator ← updateCommitteeHashValidator uch
      getAddr validator

  ds ← DistributedSet.getDs scParams
  dsInsertValidatorAddr ← do
    validator ← DistributedSet.insertValidator ds
    getAddr validator
  dsConfValidatorAddr ← do
    validator ← DistributedSet.dsConfValidator ds
    getAddr validator

  let
    mintingPolicies =
      [ "FuelMintingPolicyId" /\ fuelMintingPolicyId
      , "MPTRootTokenMintingPolicyId" /\ mptRootTokenMintingPolicyId
      , "CommitteeNftPolicyId" /\ committeeNftPolicyId
      , "DSKeyPolicy" /\ dsKeyPolicyPolicyId
      , "DSConfPolicy" /\ dsConfPolicyId
      ]

    addresses =
      [ "CommitteCandidateValidator" /\ committeeCandidateValidatorAddr
      , "MPTRootTokenValidator" /\ mptRootTokenValidatorAddr
      , "CommitteeHashValidator" /\ committeeHashValidatorAddr
      , "DSConfValidator" /\ dsConfValidatorAddr
      , "DSInsertValidator" /\ dsInsertValidatorAddr
      ]
  pure
    { addresses
    , mintingPolicies
    }

-- | Print the bech32 serialised address of a given validator
getAddr ∷ Validator → Contract () String
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
getCurrencySymbolHex ∷ MintingPolicy → Contract () String
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
