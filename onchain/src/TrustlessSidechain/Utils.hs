{-# OPTIONS_GHC -fno-specialise #-}

module TrustlessSidechain.Utils (
  verifyMultisig,
  aggregateKeys,
  aggregateCheck,
) where

import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes, getLedgerBytes))
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (EcdsaSecp256k1PubKey (getEcdsaSecp256k1PubKey))

{- | @'verifyMultisig' pubKeys threshold message signatures@ checks if at least
 @threshold@ of @pubKeys@ have signed @message@ with @signatures@.

 Preconditions

      * @signatures@ should be a subsequence of the corresponding @pubKeys@
-}
{-# INLINEABLE verifyMultisig #-}
verifyMultisig ::
  [LedgerBytes] -> Integer -> LedgerBytes -> [LedgerBytes] -> Bool
verifyMultisig pubKeys enough (LedgerBytes message) signatures = go pubKeys signatures 0
  where
    go :: [LedgerBytes] -> [LedgerBytes] -> Integer -> Bool
    go !pks !sigs !counted = case sigs of
      -- All signatures are verified, we're done
      [] -> counted >= enough
      (LedgerBytes s : ss) -> case pks of
        -- Unverified signature after checking all cases, give up
        [] -> False
        (LedgerBytes pk : pks') ->
          if verifyEcdsaSecp256k1Signature pk message s
            then -- Found a verifying key, continue
              go pks' ss (counted + 1)
            else -- Not found a verifying key yet, try again with the next one
              go pks' sigs counted

{- | 'aggregateKeys' aggregates a list of public keys into a single
 committee hash by essentially computing the merkle root of all public keys
 together.
 We call the output of this function an /aggregate public key/.
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [EcdsaSecp256k1PubKey] -> LedgerBytes
aggregateKeys = LedgerBytes . Builtins.blake2b_256 . mconcat . map (getLedgerBytes . getEcdsaSecp256k1PubKey)

{- Note [Aggregate Keys Append Scheme]
 Potential optimizations: instead of doing the concatenated hash, we could
 instead compute a merkle root.
 -}

{- | 'aggregateCheck' takes a sequence of public keys and an aggregate public
 key, and returns true or false to determinig whether the public keys were
 used to produce the aggregate public key
-}
{-# INLINEABLE aggregateCheck #-}
aggregateCheck :: [EcdsaSecp256k1PubKey] -> LedgerBytes -> Bool
aggregateCheck pubKeys avk = aggregateKeys pubKeys == avk
