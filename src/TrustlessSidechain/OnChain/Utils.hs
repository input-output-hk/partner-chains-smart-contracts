-- Validate enough signatures are signed by legit pubkeys
-- Note. this assumes the signatures are sorted (this is O(n) to check rather than O(n^2) nub)
-- Also, This will be faster if the pubkeys and signatures are in the same order
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.OnChain.Utils (verifyMulti, verifyMultisig) where

import PlutusTx.Prelude

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

{-# INLINEABLE verifyMultisig #-}
verifyMultisig :: [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
-- note. we need to test nub of either signatures or pubkeys
--   | O(n^2) nub the public keys
--   | O(n)   require public keys to be sorted then test each elem greater than last O(n)
verifyMultisig pubKeys threshold message signatures =
  let pubKeysSorted = and (zipWith (<) pubKeys (tail pubKeys)) -- insert (<) between all elements
   in pubKeysSorted && verifyMulti @BuiltinByteString @BuiltinByteString (`verifyEd25519Signature` message) threshold pubKeys signatures
