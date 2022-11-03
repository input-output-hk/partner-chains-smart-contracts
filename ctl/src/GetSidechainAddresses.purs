-- | The module `GetSidechainAddresses` provides a way to get an array of strings
-- | identifying its associated hexencoded validator and currency symbol.
module GetSidechainAddresses (SidechainAddresses, getSidechainAddresses) where

import Contract.Prelude

import CommitteCandidateValidator as CommitteCandidateValidator
import Contract.Address as Address
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
import Contract.Value as Value
import FUELMintingPolicy as FUELMintingPolicy
import MPTRoot as MPTRoot
import SidechainParams (SidechainParams)
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

type SidechainAddresses = Array (Tuple String String)

getSidechainAddresses ∷ SidechainParams → Contract () SidechainAddresses
getSidechainAddresses scParams = do
  fuelMintingPolicyId ← do
    mp ← FUELMintingPolicy.getFuelMintingPolicy scParams
    getCurrencySymbolHex mp
  mptRootTokenMintingPolicyId ← do
    mp ← MPTRoot.getMptRootTokenMintingPolicy scParams
    getCurrencySymbolHex mp
  committeeCandidateValidatorAddr ← do
    validator ← CommitteCandidateValidator.getCommitteeCandidateValidator
      scParams
    getAddr validator
  let
    addresses =
      [ "CommitteCandidateValidator" /\ committeeCandidateValidatorAddr
      , "FuelMintingPolicyId" /\ fuelMintingPolicyId
      , "MPTRootTokenMintingPolicyId" /\ mptRootTokenMintingPolicyId
      ]
  pure addresses

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

-- | `getCurrencySymbolHex` converts a mintingpolicy to its hex encoded
-- | currency symbol
getCurrencySymbolHex ∷ MintingPolicy → Contract () String
getCurrencySymbolHex mp = do
  let msg = report "getCurrencySymbolHex"
  cs ← Monad.liftContractM (msg "Cannot get currency symbol") $
    Value.scriptCurrencySymbol mp
  pure $ ByteArray.byteArrayToHex $ Value.getCurrencySymbol cs

-- | 'report' is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "GetAddresses", fun: _ }
