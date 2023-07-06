module TrustlessSidechain.Utils.Schnorr
  ( SchnorrPrivateKey(SchnorrPrivateKey)
  , SchnorrPublicKey(SchnorrPublicKey)
  , SchnorrSignature(SchnorrSignature)

  , parsePublicKey
  , parseSignature
  , generateRandomPrivateKey
  , toPubKey
  , sign
  , verify
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray

-- | Newtype wrapper around a `ByteArray`
newtype SchnorrPrivateKey = SchnorrPrivateKey ByteArray

derive instance Generic SchnorrPrivateKey _

derive instance Newtype SchnorrPrivateKey _

-- | Newtype wrapper around a `ByteArray`
newtype SchnorrPublicKey = SchnorrPublicKey ByteArray

derive instance Generic SchnorrPublicKey _

derive instance Newtype SchnorrPublicKey _

-- | Newtype wrapper around a `ByteArray`
newtype SchnorrSignature = SchnorrSignature ByteArray

derive instance Generic SchnorrSignature _

derive instance Newtype SchnorrSignature _

-- | `parsePublicKey` converts an array of bytes into a schnorr public key
-- | testing if the length is 32 bytes (the required length of a public key).
-- |
-- | Warning: this is _not_ enough to test if bytes are a valid public key. See
-- | [here](https://github.com/bitcoin-core/secp256k1/blob/332af315fcbe73f8f9dd64b4e87cfaa83d87d5b4/src/modules/extrakeys/main_impl.h#L22-L42)
-- | for details. But this should be "enough" for most purposes.
-- |
-- | See
-- | [here](https://github.com/bitcoin-core/secp256k1/blob/afd7eb4a55606bff640e30bb64bc3c43d1bb5b1c/include/secp256k1_extrakeys.h#L37-L51)
-- | for where it claims this must be 32 bytes (note the specification mentions
-- | that public keys are x coordinates)
-- |
-- | See [here](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
-- | for the specification
-- TODO: perhaps we can just do some node bindings to the underlying C library?
-- Probably the best solution instead of doing all this hacking around javascript
-- reimplementations
parsePublicKey ∷ ByteArray → Maybe SchnorrPublicKey
parsePublicKey byteArray
  | ByteArray.byteLength byteArray == 32 = Just $ SchnorrPublicKey byteArray
  | otherwise = Nothing

-- | `parseSignature` converts an array of bytes into a schnorr signature by
-- | testing if the length is 64 bytes.
-- |
-- | Warning: the simple length check probably isn't enough to verify if it
-- | really is a valid (i.e., if there exists a corresponding private key +
-- | message) for the bytes. But it hopefully is "enough" for most purposes...
-- |
-- | See
-- | [here](https://github.com/bitcoin-core/secp256k1/blob/afd7eb4a55606bff640e30bb64bc3c43d1bb5b1c/include/secp256k1_schnorrsig.h#L95-L125)
-- | for the code documenation on why this is 64 bytes.
-- |
-- | See [here](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
-- | for the specification
-- TODO: again, perhaps we can just do some node bindings to the underlying C library?
-- Probably the best solution instead of doing all this hacking around javascript
-- reimplementations
parseSignature ∷ ByteArray → Maybe SchnorrSignature
parseSignature byteArray
  | ByteArray.byteLength byteArray == 64 = Just $ SchnorrSignature byteArray
  | otherwise = Nothing

-- | Generates a random schnorr private key
generateRandomPrivateKey ∷ Effect SchnorrPrivateKey
generateRandomPrivateKey = map SchnorrPrivateKey js_randomPrivateKey

-- | Converts a schnorr private key into its corresponding public key
toPubKey ∷ SchnorrPrivateKey → SchnorrPublicKey
toPubKey (SchnorrPrivateKey ba) = SchnorrPublicKey (js_getPublicKey ba)

-- | Signs a message
sign ∷ ByteArray → SchnorrPrivateKey → SchnorrSignature
sign message (SchnorrPrivateKey privateKey) = SchnorrSignature
  (js_sign message privateKey)

-- | Verifies a signature with a public key
verify ∷ SchnorrSignature → ByteArray → SchnorrPublicKey → Boolean
verify (SchnorrSignature signature) message (SchnorrPublicKey publicKey) =
  js_verify signature message publicKey

foreign import js_randomPrivateKey ∷ Effect ByteArray

foreign import js_getPublicKey ∷ ByteArray → ByteArray

foreign import js_sign ∷ ByteArray → ByteArray → ByteArray

foreign import js_verify ∷ ByteArray → ByteArray → ByteArray → Boolean
