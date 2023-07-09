-- | `TrustlessSidechain.Utils.SchnorrSecp256k1` provides wrappers for
-- | "@noble/secp256k1"'s implementation of schnorr signatures
-- | which supposedly follows this
-- | [BIP](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki).
module TrustlessSidechain.Utils.SchnorrSecp256k1
  (
    -- type wrappers
    SchnorrSecp256k1PrivateKey(SchnorrSecp256k1PrivateKey)
  , SchnorrSecp256k1PublicKey(SchnorrSecp256k1PublicKey)
  , SchnorrSecp256k1Signature(SchnorrSecp256k1Signature)

  -- parsing / serialization
  , parsePublicKey
  , parseSignature
  , serializePublicKey
  , serializeSignature

  -- schnorr cryptography primitives
  , generateRandomPrivateKey
  , toPubKey
  , sign
  , verify
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class ToData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray

-- | Newtype wrapper around a `ByteArray`
newtype SchnorrSecp256k1PrivateKey = SchnorrSecp256k1PrivateKey ByteArray

derive instance Generic SchnorrSecp256k1PrivateKey _

derive instance Newtype SchnorrSecp256k1PrivateKey _

derive newtype instance toDataSchnorrSecp256k1PrivateKey ∷
  ToData SchnorrSecp256k1PrivateKey

derive newtype instance ordSchnorrSecp256k1PrivateKey ∷
  Ord SchnorrSecp256k1PrivateKey

derive newtype instance eqSchnorrSecp256k1PrivateKey ∷
  Eq SchnorrSecp256k1PrivateKey

-- | Newtype wrapper around a `ByteArray`
newtype SchnorrSecp256k1PublicKey = SchnorrSecp256k1PublicKey ByteArray

derive instance Generic SchnorrSecp256k1PublicKey _

derive instance Newtype SchnorrSecp256k1PublicKey _

derive newtype instance toDataSchnorrSecp256k1PublicKey ∷
  ToData SchnorrSecp256k1PublicKey

derive newtype instance ordSchnorrSecp256k1PublicKey ∷
  Ord SchnorrSecp256k1PublicKey

derive newtype instance eqSchnorrSecp256k1PublicKey ∷
  Eq SchnorrSecp256k1PublicKey

-- | Newtype wrapper around a `ByteArray`
newtype SchnorrSecp256k1Signature = SchnorrSecp256k1Signature ByteArray

derive instance Generic SchnorrSecp256k1Signature _

derive instance Newtype SchnorrSecp256k1Signature _

derive newtype instance toDataSchnorrSecp256k1Signature ∷
  ToData SchnorrSecp256k1Signature

derive newtype instance ordSchnorrSecp256k1Signature ∷
  Ord SchnorrSecp256k1Signature

derive newtype instance eqSchnorrSecp256k1Signature ∷
  Eq SchnorrSecp256k1Signature

-- | `parsePublicKey` converts an array of bytes into a schnorr public key
-- | testing if the length is 32 bytes (the required length of a public key).
--
-- Warning: this is _not_ enough to test if bytes are a valid public key. See
-- [here](https://github.com/bitcoin-core/secp256k1/blob/332af315fcbe73f8f9dd64b4e87cfaa83d87d5b4/src/modules/extrakeys/main_impl.h#L22-L42)
-- for details. But this should be "enough" for most purposes.
--
-- See
-- [here](https://github.com/bitcoin-core/secp256k1/blob/afd7eb4a55606bff640e30bb64bc3c43d1bb5b1c/include/secp256k1_extrakeys.h#L37-L51)
-- for where it claims this must be 32 bytes (note the specification mentions
-- that public keys are x coordinates)
--
-- See [here](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
-- for the specification
--
-- TODO: perhaps we can just do some node bindings to the underlying C library?
-- Probably the best solution instead of doing all this hacking around javascript
-- reimplementations
parsePublicKey ∷ ByteArray → Maybe SchnorrSecp256k1PublicKey
parsePublicKey byteArray
  | ByteArray.byteLength byteArray == 32 = Just $ SchnorrSecp256k1PublicKey
      byteArray
  | otherwise = Nothing

-- | `serializePublicKey` shows the raw bytes hex encoded.
-- Warning: it's a bit unclear if the js library follows the C library exactly
-- which requires a little bit more work to serialize public keys i.e., does it
-- do all the same work as
-- [this](https://github.com/bitcoin-core/secp256k1/blob/afd7eb4a55606bff640e30bb64bc3c43d1bb5b1c/src/modules/extrakeys/main_impl.h#L44-L57)
-- under the hood?
-- But there are tests to verify that serializing and deserializing (with
-- `parsePublicKey`) does match the underlying C library
-- used in Cardano.
--
-- Note: this follows the
-- [example](https://github.com/bitcoin-core/secp256k1/blob/master/examples/schnorr.c)
-- which also decides that it would be a good idea to print out the hex of
-- the serialization.
serializePublicKey ∷ SchnorrSecp256k1PublicKey → String
serializePublicKey (SchnorrSecp256k1PublicKey publicKey) =
  ByteArray.byteArrayToHex
    publicKey

-- | `serializeSignature` shows the raw bytes hex encoded
-- Note: this follows the
-- [example](https://github.com/bitcoin-core/secp256k1/blob/master/examples/schnorr.c)
-- which also decides that it would be a good idea to print out the hex of
-- the serialization.
serializeSignature ∷ SchnorrSecp256k1Signature → String
serializeSignature (SchnorrSecp256k1Signature publicKey) =
  ByteArray.byteArrayToHex
    publicKey

-- | `parseSignature` converts an array of bytes into a schnorr signature by
-- | testing if its length is 64 bytes.
-- |
-- Warning: the simple length check probably isn't enough to verify if it
-- really is a valid (i.e., if there exists a corresponding private key +
-- message) for the bytes. But it hopefully is "enough" for most purposes...
--
-- See
-- [here](https://github.com/bitcoin-core/secp256k1/blob/afd7eb4a55606bff640e30bb64bc3c43d1bb5b1c/include/secp256k1_schnorrsig.h#L95-L125)
-- for the code documenation on why this is 64 bytes.
--
-- See [here](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
-- for the specification
--
-- TODO: again, perhaps we can just do some node bindings to the underlying C library?
-- Probably the best solution instead of doing all this hacking around javascript
-- reimplementations
parseSignature ∷ ByteArray → Maybe SchnorrSecp256k1Signature
parseSignature byteArray
  | ByteArray.byteLength byteArray == 64 = Just $ SchnorrSecp256k1Signature
      byteArray
  | otherwise = Nothing

-- | Generates a random schnorr private key
generateRandomPrivateKey ∷ Effect SchnorrSecp256k1PrivateKey
generateRandomPrivateKey = map SchnorrSecp256k1PrivateKey js_randomPrivateKey

-- | Converts a schnorr private key into its corresponding public key
toPubKey ∷ SchnorrSecp256k1PrivateKey → SchnorrSecp256k1PublicKey
toPubKey (SchnorrSecp256k1PrivateKey ba) = SchnorrSecp256k1PublicKey
  (js_getPublicKey ba)

-- | Signs a message
sign ∷ ByteArray → SchnorrSecp256k1PrivateKey → SchnorrSecp256k1Signature
sign message (SchnorrSecp256k1PrivateKey privateKey) = SchnorrSecp256k1Signature
  (js_sign message privateKey)

-- | Verifies a signature with a public key
verify ∷
  SchnorrSecp256k1Signature → ByteArray → SchnorrSecp256k1PublicKey → Boolean
verify
  (SchnorrSecp256k1Signature signature)
  message
  (SchnorrSecp256k1PublicKey publicKey) =
  js_verify signature message publicKey

foreign import js_randomPrivateKey ∷ Effect ByteArray

foreign import js_getPublicKey ∷ ByteArray → ByteArray

foreign import js_sign ∷ ByteArray → ByteArray → ByteArray

foreign import js_verify ∷ ByteArray → ByteArray → ByteArray → Boolean
