module Utils.Crypto
  ( toPubKeyUnsafe
  , sign
  , verifyEd25519Signature
  , generatePrivKey
  , multiSign
  , hexToPrivKeyUnsafe
  , normalizeCommitteePubKeysAndSignatures
  , verifyMultiSignature
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToIntArray
  , hexToByteArrayUnsafe
  )
import Data.Array as Array
import Data.Int as Int
import Data.Ord as Ord
import Serialization.Types (PrivateKey, PublicKey)
import Types (PubKey, Signature)

foreign import publicKeyFromPrivateKeyUnsafe ∷ PrivateKey → PublicKey
foreign import publicKeyToBytesUnsafe ∷ PublicKey → ByteArray
foreign import generateRandomBIP32PrivateKeyArrayInt8 ∷ Effect (Array Int)
foreign import generateBIP32PrivateKeyFromArray ∷ Array Int → PrivateKey
foreign import sign ∷ PrivateKey → ByteArray → Signature
foreign import verifyEd25519Signature ∷
  PubKey → ByteArray → Signature → Boolean

multiSign ∷ Array PrivateKey → ByteArray → Array Signature
multiSign xkeys msg = map (flip sign msg) xkeys

hexToPrivKeyUnsafe ∷ String → PrivateKey
hexToPrivKeyUnsafe =
  hexToByteArrayUnsafe >>> byteArrayToIntArray >>>
    generateBIP32PrivateKeyFromArray

generatePrivKey ∷ Contract () PrivateKey
generatePrivKey =
  liftEffect $ generateBIP32PrivateKeyFromArray <$>
    generateRandomBIP32PrivateKeyArrayInt8

toPubKeyUnsafe ∷ PrivateKey → PubKey
toPubKeyUnsafe = publicKeyToBytesUnsafe <<< publicKeyFromPrivateKeyUnsafe

-- | 'normalizeCommitteePubKeysAndSignatures' takes a list of public keys and their
-- associated signatures, sorts by the natural lexicographical ordering of the
-- public keys, then unzips the resulting array, removing all signatures that
-- are 'Nothing'.
--
-- This useful since the onchain multisign method (see in the Haskell module
-- 'TrustlessSidechain.OnChain.Utils') requires that the keys are sorted (this
-- makes testing if the list is nubbed easy), and the signatures are associated
-- with the public keys
normalizeCommitteePubKeysAndSignatures ∷
  Array (PubKey /\ Maybe Signature) → Tuple (Array PubKey) (Array Signature)
normalizeCommitteePubKeysAndSignatures =
  (Array.catMaybes <$> _) -- apply @Array.catMaybes@ over the second element of the tuple.

    <<< Array.unzip
    <<< Array.sortBy (\l r → Ord.compare (fst l) (fst r))

-- | > @'verifyMultiSignature' thresholdNumerator thresholdDenominator pubKeys msg signatures@
-- returns true iff
--
--      - @pubKeys@ is sorted lexicographically and are distinct
--
--      - @signatures@ are the corresponding signatures @pubKeys@ of @msg@
--      as a subsequence of @pubKeys@ (i.e., ordered the same way as @pubKeys@).
--
--      - strictly more than @thresholdNumerator/thresholdDenominator@
--      @pubKeys@ have signed @msg@
--
-- Note: this loosely replicates the behavior of the corresponding on chain
-- function, but should be significantly more efficient (since we use the
-- assumption that the signatures are essentially a subsequence of the public
-- keys); and is generalized to allow arbitrary thresholds to be given.
verifyMultiSignature ∷
  Int → Int → Array PubKey → ByteArray → Array Signature → Boolean
verifyMultiSignature
  thresholdNumerator
  thresholdDenominator
  pubKeys
  msg
  signatures =
  let
    go ∷ Int → Array PubKey → Array Signature → Boolean
    go signed pubs sigs =
      let
        ok = signed >
          ( Int.quot (thresholdNumerator * Array.length pubKeys)
              thresholdDenominator
          )
      in
        case Array.uncons pubs of
          Nothing → ok
          Just { head: pub, tail: pubs' } →
            case Array.uncons sigs of
              Nothing → ok
              Just { head: sig, tail: sigs' } →
                if verifyEd25519Signature pub msg sig then
                  -- the public key and signature match, so
                  -- we move them both forward..
                  go (signed + 1) pubs' sigs'

                else
                  -- otherwise, they don't match so since
                  -- @sigs@ is essentially a subsequence of
                  -- @pubs@, we move only @pubs@ forward
                  -- since a later key should match with
                  -- @sig@.
                  go signed pubs' sigs
  in
    go 0 pubKeys signatures
