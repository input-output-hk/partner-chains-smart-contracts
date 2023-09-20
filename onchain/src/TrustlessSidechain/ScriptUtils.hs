{-# LANGUAGE GADTs #-}

module TrustlessSidechain.ScriptUtils (
  mkUntypedValidator,
  mkUntypedMintingPolicy,
  scriptSize,
  scriptToPlutusScript,
) where

import TrustlessSidechain.PlutusPrelude

import Cardano.Api (PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Kind (Type)
import Plutonomy.UPLC qualified
import Plutus.V1.Ledger.Scripts (Script (Script))
import Plutus.V2.Ledger.Contexts (ScriptContext)
import UntypedPlutusCore qualified as UPLC

{- | Convert a validator to untyped
 The output will accept BuiltinData instead of concrete types
-}
mkUntypedValidator ::
  forall (d :: Type) (r :: Type).
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
  check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

{- | Convert a minting policy to untyped
 The output will accept BuiltinData instead of concrete types
-}
mkUntypedMintingPolicy ::
  forall (r :: Type).
  (UnsafeFromData r) =>
  (r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedMintingPolicy f r p =
  check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

scriptSize :: Script -> Integer
scriptSize (Script x) = UPLC.programSize x

scriptToPlutusScript :: Script -> PlutusScript PlutusScriptV2
scriptToPlutusScript =
  PlutusScriptSerialised @PlutusScriptV2
    . toShort
    . toStrict
    . serialise
    . Plutonomy.UPLC.optimizeUPLC
