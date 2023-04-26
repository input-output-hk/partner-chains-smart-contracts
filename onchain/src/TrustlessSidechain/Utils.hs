{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Validate enough signatures are signed by legit pubkeys
{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (verifyMultisig, aggregateKeys, aggregateCheck) where

import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import TrustlessSidechain.Types (SidechainPubKey (getSidechainPubKey))

{- | @'verifyMultisig' pubKeys threshold message signatures@ checks if at least
 @threshold@ of @pubKeys@ have signed @message@ with @signatures@.

 Preconditions

      * @signatures@ should be a subsequence of the corresponding @pubKeys@
-}
{-# INLINEABLE verifyMultisig #-}
verifyMultisig :: [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
verifyMultisig pubKeys threshold message signatures =
  let go :: Integer -> [BuiltinByteString] -> [BuiltinByteString] -> Bool
      go !signed !pubs !sigs =
        let ok = signed >= threshold
         in ok
              || ( case pubs of
                    [] -> ok
                    pub : pubs' ->
                      case sigs of
                        [] -> ok
                        sig : sigs' ->
                          if verifyEcdsaSecp256k1Signature pub message sig
                            then -- the public key and signature match, so
                            -- we move them both forward..
                              go (signed + 1) pubs' sigs'
                            else -- otherwise, they don't match so since
                            -- `sigs` is essentially a subsequence of
                            -- `pubs`, we move only `pubs` forward
                            -- since a later key should match with
                            -- `sig`.
                              go signed pubs' sigs
                 )
   in go 0 pubKeys signatures

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
