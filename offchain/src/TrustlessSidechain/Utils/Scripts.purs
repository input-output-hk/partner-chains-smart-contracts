module TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  , mkMintingPolicyWithParams
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
import Data.Either (note)
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript, InvalidScriptArgs)
  )

-- | `mkValidatorWithParams hexScript params` returns the `Validator` of
-- | `hexScript` with the script applied to `params`.  This is a convenient
-- | alias to help create the distributed set validators.
mkValidatorWithParams ∷
  String →
  Array PlutusData →
  Either InternalError Validator
mkValidatorWithParams hexScript params = do
  unapplied ← note (InvalidScript hexScript)
    (decodeTextEnvelope hexScript >>= plutusScriptV2FromEnvelope)
  applied ←
    if Array.null params then pure unapplied
    else (InvalidScriptArgs <<< unwrap) `lmap` Scripts.applyArgs unapplied params
  pure $ Validator applied

-- | `mkMintingPolicyWithParams hexScript params` returns the `MintingPolicy` of `hexScript`
-- | with the script applied to `params`. This is a convenient alias
-- | to help create the distributed set minting policies.
mkMintingPolicyWithParams ∷
  String →
  Array PlutusData →
  Either InternalError MintingPolicy
mkMintingPolicyWithParams hexScript params = do
  unapplied ← note (InvalidScript hexScript)
    (decodeTextEnvelope hexScript >>= plutusScriptV2FromEnvelope)
  applied ←
    if Array.null params then pure unapplied
    else (InvalidScriptArgs <<< unwrap) `lmap` Scripts.applyArgs unapplied params
  pure $ PlutusMintingPolicy applied
