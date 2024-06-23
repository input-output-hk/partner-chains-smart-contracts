module Test.InitSidechain.Utils
  ( expectedInitTokens
  , failMsg
  , unorderedEq
  ) where

import Prelude

import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusScript (PlutusScript)
import Contract.Prelude (Tuple, foldr, (/\))
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map as Map
import TrustlessSidechain.Versioning.ScriptId as Types
import TrustlessSidechain.Versioning.Utils as Versioning

-- | Testing utility to check ordered equality of
-- | Map.Map, whose Eq instance is derived from the Array Eq instance
-- | and therefore is sensitive to the order of insertion.
-- | Note this is not *set* equality, since there is no deduplication.
unorderedEq ∷
  ∀ k v.
  Ord k ⇒
  Ord v ⇒
  Map.Map k v →
  Map.Map k v →
  Boolean
unorderedEq m1 m2 =
  let
    kvs m = Array.sort $ Map.toUnfoldable m
  in
    kvs m1 == kvs m2

-- | Testing utility for showing expected/actual
failMsg ∷ ∀ a b. Show a ⇒ Show b ⇒ a → b → String
failMsg exp act = "Expected: "
  <> show exp
  <> "\nBut got: "
  <> show act

-- | Collection of init tokens expected to be minted by
-- | `initTokensMint`. It does not care about
-- | ATMSKinds or the particular version, just the token name
-- | and quantity. Requires the number of version oracle init tokens
-- | to be passed.
expectedInitTokens ∷
  Int → -- How many version init tokens should have been burned at this point?
  List (Tuple Types.ScriptId PlutusScript) →
  List (Tuple Types.ScriptId PlutusScript) →
  Array AssetName →
  Map.Map AssetName BigNum
expectedInitTokens tokensUsed versionedPolicies versionedValidators tokens =
  let
    -- See `Versioning.mintVersionInitTokens` for where this comes from
    nversion = List.length versionedPolicies
      + List.length versionedValidators
  in
    foldr (\(k /\ v) → Map.insert k v) Map.empty
      $ Array.(:)
          ( Versioning.versionOracleInitTokenName /\
              (BigNum.fromInt (nversion - tokensUsed))
          )
      $
        map
          (_ /\ BigNum.fromInt 1)
          tokens
