module TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  , getDParameterValidatorAndAddress
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , getNetworkId
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData
  ( toData
  )
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  , validatorHash
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import TrustlessSidechain.RawScripts
  ( rawDParameterMintingPolicy
  , rawDParameterValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Utils (getVersionOracleConfig) as Versioning

-- | Get the DummyMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDParameterMintingPolicy ∷ SidechainParams → Contract MintingPolicy
decodeDParameterMintingPolicy sidechainParams = do
  let
    script = decodeTextEnvelope rawDParameterMintingPolicy >>=
      plutusScriptV2FromEnvelope
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ Scripts.applyArgs unapplied
    [ toData sidechainParams, toData versionOracleConfig ]
  pure $ PlutusMintingPolicy applied

decodeDParameterValidator ∷ SidechainParams → Contract Validator
decodeDParameterValidator sidechainParams = do
  let
    script = decodeTextEnvelope rawDParameterValidator >>=
      plutusScriptV2FromEnvelope
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ Scripts.applyArgs unapplied
    [ toData sidechainParams, toData versionOracleConfig ]
  pure $ Validator applied

getDParameterValidatorAndAddress ∷
  SidechainParams →
  Contract
    { dParameterValidator ∷ Validator
    , dParameterValidatorAddress ∷ Address
    }
getDParameterValidatorAndAddress sidechainParams = do
  dParameterValidator ← decodeDParameterValidator sidechainParams
  netId ← getNetworkId
  dParameterValidatorAddress ←
    liftContractM "cannot get d parameter validator address"
      (validatorHashEnterpriseAddress netId (validatorHash dParameterValidator))

  pure { dParameterValidator, dParameterValidatorAddress }

getDParameterMintingPolicyAndCurrencySymbol ∷
  SidechainParams →
  Contract
    { dParameterMintingPolicy ∷ MintingPolicy
    , dParameterCurrencySymbol ∷ CurrencySymbol
    }
getDParameterMintingPolicyAndCurrencySymbol sidechainParams = do
  dParameterMintingPolicy ← decodeDParameterMintingPolicy sidechainParams
  dParameterCurrencySymbol ← liftContractM
    "Failed to get DParameterMintingPolicy"
    (Value.scriptCurrencySymbol dParameterMintingPolicy)
  pure { dParameterMintingPolicy, dParameterCurrencySymbol }
