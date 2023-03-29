-- Validate enough signatures are signed by legit pubkeys
-- Note. this assumes the signatures are sorted (this is O(n) to check rather than O(n^2) nub)
-- Also, This will be faster if the pubkeys and signatures are in the same order
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (verifyMulti, verifyMultisig, aggregateKeys, aggregateCheck) where

import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import TrustlessSidechain.Types (SidechainPubKey (getSidechainPubKey))

-- ? can we require a single list [(Pubkey , Signature)] to validate

{-# INLINEABLE verifyMulti #-}
-- Check enough pubkeys match a signature without counting duplicate pubkeys or signatures (potential security risk)
verifyMulti :: Eq b => (a -> b -> Bool) -> Integer -> [a] -> [b] -> Bool
verifyMulti isOK threshold pubKeys signatures =
  -- discard signatures once matched. looping the sig list should be faster since it should be the smaller list
  -- threads the updated siglist for subsequent pubkeys
  let didPubkeySign pk = \case
        [] -> Nothing
        s : sigs | isOK pk s -> Just sigs -- discard matched signature and report match
        s : sigs -> (s :) <$> didPubkeySign pk sigs -- don't discard
   in threshold <= 0 || case pubKeys of
        [] -> False
        p : pks -> case didPubkeySign p signatures of
          Nothing -> verifyMulti isOK threshold pks signatures -- KO skip pubkey and reuse siglist
          Just sgs -> verifyMulti isOK (threshold - 1) pks sgs -- OK update threshold and siglist

{- | @'verifyMultisig' pubKeys threshold message signatures@ checks if at least
 @threshold@ of @pubKeys@ have signed @message@ with @signatures@.

 Preconditions

      * @signatures@ should be sorted (otherwise this returns False)
-}
{-# INLINEABLE verifyMultisig #-}
verifyMultisig :: [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
-- note. we need to test nub of either signatures or pubkeys
--   | O(n^2) nub the public keys
--   | O(n)   require public keys to be sorted then test each elem greater than last O(n)
verifyMultisig pubKeys threshold message signatures =
  let pubKeysSorted = and (zipWith (<) pubKeys (tail pubKeys)) -- insert (<) between all elements
   in pubKeysSorted && verifyMulti (`verifyEcdsaSecp256k1Signature` message) threshold pubKeys signatures

{- | 'aggregateKeys' aggregates a list of public keys into a single
 committee hash by essentially computing the merkle root of all public keys
 together.
 We call the output of this function an /aggregate public key/.
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [SidechainPubKey] -> BuiltinByteString
aggregateKeys = Builtins.blake2b_256 . mconcat . map getSidechainPubKey

{- Note [Aggregate Keys Append Scheme]
 Potential optimizations: instead of doing the concatenated hash, we could
 instead compute a merkle root.
 -}

{- | 'aggregateCheck' takes a sequence of public keys and an aggregate public
 key, and returns true or false to determinig whether the public keys were
 used to produce the aggregate public key
-}
{-# INLINEABLE aggregateCheck #-}
aggregateCheck :: [SidechainPubKey] -> BuiltinByteString -> Bool
aggregateCheck pubKeys avk = aggregateKeys pubKeys == avk
