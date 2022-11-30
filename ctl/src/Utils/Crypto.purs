module Utils.Crypto
  ( Message
  , PrivateKey
  , PublicKey
  , Signature
  , toPubKeyUnsafe
  , generatePrivKey
  , multiSign
  , sign
  , verifyEcdsaSecp256k1Signature
  , normalizeCommitteePubKeysAndSignatures
  , verifyMultiSignature
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (on)
import Data.Ord as Ord
import Types (PubKey)

-- | Invariant: ∀ x : PublicKey. length x = 33
-- | Format: Compressed & Serialized as per secp256k1 implementation
-- make sure to check the leading byte for valid key format
type PublicKey = ByteArray

-- | Invariant: ∀ x : PrivateKey. length x = 32
-- | Format: raw bytes
type PrivateKey = ByteArray

-- | Invariant: ∀ x : Message. length x = 32
-- | Format: raw bytes
type Message = ByteArray

-- | Invariant: ∀ x : Signature. length x = 64
type Signature = ByteArray

-- TODO: newtype checks the type aliases above

foreign import generateRandomPrivateKey ∷ Effect PrivateKey
foreign import toPubKeyUnsafe ∷ PrivateKey → PublicKey
foreign import sign ∷ Message → PrivateKey → Signature
foreign import verifyEcdsaSecp256k1Signature ∷
  PublicKey → Message → Signature → Boolean

generatePrivKey ∷ Contract () PrivateKey
generatePrivKey =
  liftEffect generateRandomPrivateKey

multiSign ∷ Array PrivateKey → Message → Array Signature
multiSign xkeys msg = map (sign msg) xkeys

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
  map Array.catMaybes <<< Array.unzip <<< Array.sortBy (Ord.compare `on` fst)

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
  BigInt → BigInt → Array PubKey → ByteArray → Array Signature → Boolean
verifyMultiSignature
  thresholdNumerator
  thresholdDenominator
  pubKeys
  msg
  signatures =
  let
    go ∷ BigInt → Array PubKey → Array Signature → Boolean
    go signed pubs sigs =
      let
        ok = signed >
          ( BigInt.quot
              (thresholdNumerator * BigInt.fromInt (Array.length pubKeys))
              thresholdDenominator
          )
      in
        case Array.uncons pubs of
          Nothing → ok
          Just { head: pub, tail: pubs' } →
            case Array.uncons sigs of
              Nothing → ok
              Just { head: sig, tail: sigs' } →
                if verifyEcdsaSecp256k1Signature pub msg sig then
                  -- the public key and signature match, so
                  -- we move them both forward..
                  go (signed + one) pubs' sigs'

                else
                  -- otherwise, they don't match so since
                  -- @sigs@ is essentially a subsequence of
                  -- @pubs@, we move only @pubs@ forward
                  -- since a later key should match with
                  -- @sig@.
                  go signed pubs' sigs
  in
    isSorted pubKeys && go zero pubKeys signatures

{- | Verifies that the non empty array is sorted -}
isSorted ∷ ∀ a. Ord a ⇒ Array a → Boolean
isSorted xss = case Array.tail xss of
  Just xs → and (Array.zipWith (<) xss xs) -- insert (<) between all elements
  Nothing → false
