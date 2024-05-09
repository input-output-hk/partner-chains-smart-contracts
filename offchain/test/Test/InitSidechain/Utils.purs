module Test.InitSidechain.Utils
  ( expectedInitTokens
  , failMsg
  , unorderedEq
  ) where

import Prelude

import Contract.AssocMap as Plutus.Map
import Contract.Prelude (Tuple, foldr, (/\))
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Value (TokenName)
import Ctl.Internal.Types.TokenName as Value
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.BigInt as BigInt
import Data.List (List)
import Data.List as List
import TrustlessSidechain.Versioning.ScriptId as Types
import TrustlessSidechain.Versioning.Utils as Versioning

-- | Testing utility to check ordered equality of
-- | Plutus.Map.Map, whose Eq instance is derived from the Array Eq instance
-- | and therefore is sensitive to the order of insertion.
-- | Note this is not *set* equality, since there is no deduplication.
unorderedEq ∷
  ∀ k v.
  Ord k ⇒
  Ord v ⇒
  Plutus.Map.Map k v →
  Plutus.Map.Map k v →
  Boolean
unorderedEq m1 m2 =
  let
    kvs m = Array.sort $ Array.zip (Plutus.Map.keys m)
      (Plutus.Map.elems m)
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
  List (Tuple Types.ScriptId MintingPolicy) →
  List (Tuple Types.ScriptId Validator) →
  Array TokenName →
  Plutus.Map.Map Value.TokenName BigInt.BigInt
expectedInitTokens tokensUsed versionedPolicies versionedValidators tokens =
  let
    -- See `Versioning.mintVersionInitTokens` for where this comes from
    nversion = BigInt.fromInt $ List.length versionedPolicies
      + List.length versionedValidators
  in
    foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
      $ Array.(:)
          ( Versioning.versionOracleInitTokenName /\
              (nversion - fromInt tokensUsed)
          )
      $
        map
          (_ /\ one)
          tokens
