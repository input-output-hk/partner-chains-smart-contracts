module TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  , mkMintingPolicyWithParams
  ) where

import Contract.Prelude

import Contract.PlutusData (PlutusData)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Except
import TrustlessSidechain.Utils.Error
  ( InternalError(InvalidScriptEnvelope, InvalidScriptArgs)
  )
import Type.Row (type (+))

-- | `mkValidatorWithParams hexScript params` returns the `Validator` of
-- | `hexScript` with the script applied to `params`.  This is a convenient
-- | alias to help create the distributed set validators.
mkValidatorWithParams ∷
  ∀ r.
  String →
  Array PlutusData →
  Run (EXCEPT InternalError + r) Validator
mkValidatorWithParams hexScript params = do
  let
    script = decodeTextEnvelope hexScript >>= plutusScriptV2FromEnvelope

  unapplied ← Except.note (InvalidScriptEnvelope hexScript) script
  applied ←
    if Array.null params then pure unapplied
    else Except.rethrow $
      InvalidScriptArgs `lmap` Scripts.applyArgs unapplied params
  pure $ Validator applied

-- | `mkMintingPolicyWithParams hexScript params` returns the `MintingPolicy` of `hexScript`
-- | with the script applied to `params`. This is a convenient alias
-- | to help create the distributed set minting policies.
mkMintingPolicyWithParams ∷
  ∀ r.
  String →
  Array PlutusData →
  Run (EXCEPT InternalError + r) MintingPolicy
mkMintingPolicyWithParams hexScript params = do
  let
    script = decodeTextEnvelope hexScript >>= plutusScriptV2FromEnvelope

  unapplied ← Except.note (InvalidScriptEnvelope hexScript) script
  applied ←
    if Array.null params then pure unapplied
    else Except.rethrow $
      InvalidScriptArgs `lmap` Scripts.applyArgs unapplied params
  pure $ PlutusMintingPolicy applied
