module TrustlessSidechain.OnChain.Utils where

import PlutusTx.Prelude (Bool (..), BuiltinByteString, Integer, any, const, filter, id, not, null, verifySignature, (-), (.), (<=))

verifyMultisig :: [BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool
verifyMultisig pubKeys threshold message signatures =
  let didPubkeySign pk = any (verifySignature pk message) signatures
   in lengthAtLeast threshold (filter didPubkeySign pubKeys)

-- does not force evaluation of the entire list
lengthAtLeast :: Integer -> [a] -> Bool
lengthAtLeast n = if n <= 0 then const True else not . null . drop (n - 1)

-- Plutus doesn't have drop for some reason
drop :: Integer -> [a] -> [a]
drop n | n <= 0 = id
drop n = \case
  [] -> []
  _ : xs -> drop (n - 1) xs
