module TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  , mkValidatorWithParams'
  , mkMintingPolicyWithParams
  , mkMintingPolicyWithParams'
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData (PlutusData)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Map as Map
import TrustlessSidechain.Error
  ( InternalError
      ( InvalidScriptEnvelope
      , InvalidScriptArgs
      , InvalidScriptId
      )
  )
import TrustlessSidechain.RawScripts (rawScripts)
import TrustlessSidechain.Versioning.ScriptId (ScriptId)

-- | `mkValidatorWithParams scriptId params` returns the `Validator` of
-- | `scriptId` with the script applied to `params`.
mkValidatorWithParams ∷
  ScriptId →
  Array PlutusData →
  Contract Validator
mkValidatorWithParams scriptId params = do
  hexScript ← liftContractM (show $ InvalidScriptId scriptId)
    (Map.lookup scriptId rawScripts)
  mkValidatorWithParams' hexScript params

-- | `mkValidatorWithParams' hexScript params` returns the `Validator` of
-- | `hexScript` with the script applied to `params`.
mkValidatorWithParams' ∷
  String →
  Array PlutusData →
  Contract Validator
mkValidatorWithParams' hexScript params = do
  let
    script = decodeTextEnvelope hexScript >>= plutusScriptV2FromEnvelope

  unapplied ← liftContractM (show $ InvalidScriptEnvelope hexScript) script
  applied ←
    if Array.null params then pure unapplied
    else liftContractE $
      (show <<< InvalidScriptArgs) `lmap` Scripts.applyArgs unapplied params
  pure $ Validator applied

-- | `mkMintingPolicyWithParams scriptId params` returns the `MintingPolicy` of
-- | `scriptId` with the script applied to `params`.
mkMintingPolicyWithParams ∷
  ScriptId →
  Array PlutusData →
  Contract MintingPolicy
mkMintingPolicyWithParams scriptId params = do
  hexScript ← liftContractM (show $ InvalidScriptId scriptId)
    (Map.lookup scriptId rawScripts)
  mkMintingPolicyWithParams' hexScript params

-- | `mkMintingPolicyWithParams' hexScript params` returns the `MintingPolicy`
-- | of `hexScript` with the script applied to `params`.
mkMintingPolicyWithParams' ∷
  String →
  Array PlutusData →
  Contract MintingPolicy
mkMintingPolicyWithParams' hexScript params = do
  let
    script = decodeTextEnvelope hexScript >>= plutusScriptV2FromEnvelope

  unapplied ← liftContractM (show $ InvalidScriptEnvelope hexScript) script
  applied ←
    if Array.null params then pure unapplied
    else liftContractE $
      (show <<< InvalidScriptArgs) `lmap` Scripts.applyArgs unapplied params
  pure $ PlutusMintingPolicy applied
