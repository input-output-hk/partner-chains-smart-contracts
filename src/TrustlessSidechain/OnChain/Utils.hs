module TrustlessSidechain.OnChain.Utils where

import PlutusTx.Prelude (Bool (..), BuiltinByteString, Integer, Maybe (..), nub, verifySignature, (||) , (-), (<$>), (<=))

-- Check enough pubkeys match a signature without counting duplicate pubkeys or signatures (potential security risk)
verifyMultisig :: [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
verifyMultisig pubKeys threshold message signatures =
  -- discard signatures once matched. looping the sig list should be faster since it should be the smaller list
  -- threads the updated siglist for subsequent pubkeys
  let didPubkeySign :: BuiltinByteString -> [BuiltinByteString] -> Maybe [BuiltinByteString]
      didPubkeySign pk = \case
        [] -> Nothing
        s : sigs | verifySignature pk message s -> Just sigs -- discard matched signature
        s : sigs -> (s :) <$> didPubkeySign pk sigs -- don't discard
      tryPubKeys (needed, _) [] = needed <= 0
      tryPubKeys (needed, sigs) (pubkey : nextPKs) =
        needed <= 0 || case didPubkeySign pubkey sigs of
          Nothing -> tryPubKeys (needed, sigs) nextPKs -- KO skip this pubkey and reuse same sig list
          Just sigs' -> tryPubKeys (needed - 1, sigs') nextPKs -- OK update threshold and sigs list
          -- note. we don't need to nub both sigs and pubkeys which would waste time in general
   in tryPubKeys (threshold, nub signatures) pubKeys
