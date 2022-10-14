module Utils.Crypto
  ( toPubKeyUnsafe
  , sign
  , verifyEd25519Signature
  , generatePrivKey
  , multiSign
  , hexToPrivKeyUnsafe
  , normalizeCommitteePubKeysAndSignatures
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToIntArray
  , hexToByteArrayUnsafe
  )
import Data.Array as Array
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
