module TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  , mkMintingPolicyWithParams
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData (class ToData, toData)
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  )
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Data.Array as Array

-- | `mkValidatorWithParams hexScript params` returns the `Validator` of `hexScript`
-- | with the script applied to `params`. This is a convenient alias
-- | to help create the distributed set validators.
mkValidatorWithParams ∷
  ∀ (a ∷ Type).
  ToData a ⇒
  String →
  Array a →
  Contract Validator
mkValidatorWithParams hexScript params = do
  let
    script = decodeTextEnvelope hexScript
      >>= plutusScriptV2FromEnvelope

  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ←
    if Array.null params then pure unapplied
    else liftContractE $ Scripts.applyArgs unapplied $ map toData params
  pure $ Validator applied

-- | `mkMintingPolicyWithParams hexScript params` returns the `MintingPolicy` of `hexScript`
-- | with the script applied to `params`. This is a convenient alias
-- | to help create the distributed set minting policies.
mkMintingPolicyWithParams ∷
  ∀ (a ∷ Type).
  ToData a ⇒
  String →
  Array a →
  Contract MintingPolicy
mkMintingPolicyWithParams hexScript params = do
  let
    script = decodeTextEnvelope hexScript
      >>= plutusScriptV2FromEnvelope

  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ←
    if Array.null params then pure unapplied
    else liftContractE $ Scripts.applyArgs unapplied $ map toData params
  pure $ PlutusMintingPolicy applied
