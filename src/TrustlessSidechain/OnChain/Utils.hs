module TrustlessSidechain.OnChain.Utils where

import PlutusTx.Prelude (Bool (..), BuiltinByteString, Integer, Maybe (..), foldl, fst, nub, verifySignature, (-), (<$>), (<=))

-- Check enough pubkeys match a signature without counting duplicate pubkeys or signatures (potential security risk)
verifyMultisig :: [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
verifyMultisig pubKeys threshold message signatures =
  -- discard signatures once matched. looping the sig list should be faster since it should be the smaller list
  -- threads the updated siglist for foldl
  let didPubkeySign :: BuiltinByteString -> [BuiltinByteString] -> Maybe [BuiltinByteString]
      didPubkeySign pk = \case
        [] -> Nothing
        s : sigs | verifySignature pk message s -> Just sigs -- discard matched signature
        s : sigs -> (s :) <$> didPubkeySign pk sigs -- don't discard
        -- fn passed to foldl: state is the sigs needed and the siglist minus already matched sigs
      tryPubKey :: (Integer, [BuiltinByteString]) -> BuiltinByteString -> (Integer, [BuiltinByteString])
      tryPubKey acc@(needed, sigs) pubkey =
        if needed <= 0
          then acc -- terminate with success (ie. by not inspecting pubkey)
          else case didPubkeySign pubkey sigs of
            Nothing -> (needed, sigs) -- KO skip this pubkey and retry with same sig list
            Just sigs' -> (needed - 1, sigs') -- OK update threshold and sigs list
            -- foldl needed so we can terminate early
            -- note. we don't need to nub both sigs and pubkeys which would waste time in general
   in fst (foldl tryPubKey (threshold, nub signatures) pubKeys) <= 0
