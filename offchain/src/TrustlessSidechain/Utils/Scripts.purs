module TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  , mkValidatorWithParams'
  , mkMintingPolicyWithParams
  , mkMintingPolicyWithParams'
  ) where
import Cardano.Types.PlutusScript (PlutusScript)
import Contract.Prelude hiding (note)
import Contract.PlutusData (PlutusData)
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT, note, rethrow)
import TrustlessSidechain.Error
  ( OffchainError
      ( InvalidScript
      , InvalidScriptArgs
      , InvalidScriptId
      )
  )
import TrustlessSidechain.RawScripts (rawScripts)
import TrustlessSidechain.Versioning.ScriptId (ScriptId)
import Type.Row (type (+))

-- | `mkValidatorWithParams scriptId params` returns the `Validator` of
-- | `scriptId` with the script applied to `params`.
mkValidatorWithParams ∷
  ∀ r.
  ScriptId →
  Array PlutusData →
  Run (EXCEPT OffchainError + r) PlutusScript
mkValidatorWithParams scriptId params = do
  hexScript ← note (InvalidScriptId scriptId)
    (Map.lookup scriptId rawScripts)
  mkValidatorWithParams' hexScript params

-- | `mkValidatorWithParams' hexScript params` returns the `Validator` of
-- | `hexScript` with the script applied to `params`.
mkValidatorWithParams' ∷
  ∀ r.
  String →
  Array PlutusData →
  Run (EXCEPT OffchainError + r) PlutusScript
mkValidatorWithParams' hexScript params = do
  let
    script = decodeTextEnvelope hexScript >>= plutusScriptFromEnvelope

  unapplied ← note (InvalidScript hexScript) script
  applied ←
    rethrow $
      if Array.null params then pure unapplied
      else
        InvalidScriptArgs `lmap` Scripts.applyArgs unapplied params
  pure $ applied

-- | `mkMintingPolicyWithParams scriptId params` returns the `MintingPolicy` of
-- | `scriptId` with the script applied to `params`.
mkMintingPolicyWithParams ∷
  ∀ r.
  ScriptId →
  Array PlutusData →
  Run (EXCEPT OffchainError + r) PlutusScript
mkMintingPolicyWithParams scriptId params = do
  hexScript ← note (InvalidScriptId scriptId)
    (Map.lookup scriptId rawScripts)
  mkMintingPolicyWithParams' hexScript params

-- | `mkMintingPolicyWithParams' hexScript params` returns the `MintingPolicy`
-- | of `hexScript` with the script applied to `params`.
mkMintingPolicyWithParams' ∷
  ∀ r.
  String →
  Array PlutusData →
  Run (EXCEPT OffchainError + r) PlutusScript
mkMintingPolicyWithParams' hexScript params = do
  let
    script = decodeTextEnvelope hexScript >>= plutusScriptFromEnvelope
  unapplied ← note (InvalidScript hexScript) script
  applied ← rethrow $
    if Array.null params then pure unapplied
    else (InvalidScriptArgs) `lmap` Scripts.applyArgs unapplied params
  pure $ applied
