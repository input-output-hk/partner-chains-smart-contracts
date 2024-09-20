module TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1Message(EcdsaSecp256k1Message)
  , ecdsaSecp256k1Message
  , byteArrayToEcdsaSecp256k1MessageUnsafe
  , EcdsaSecp256k1PrivateKey
  , byteArrayToEcdsaSecp256k1PubKeyUnsafe
  , EcdsaSecp256k1PubKey(EcdsaSecp256k1PubKey)
  , EcdsaSecp256k1Signature(EcdsaSecp256k1Signature)
  , toPubKeyUnsafe
  , generateRandomPrivateKey
  , sign
  , ecdsaSecp256k1PubKey
  , getEcdsaSecp256k1PubKeyByteArray
  , getEcdsaSecp256k1PrivateKeyByteArray
  , getEcdsaSecp256k1SignatureByteArray
  , byteArrayToEcdsaSecp256k1PrivateKeyUnsafe
  , ecdsaSecp256k1PrivateKey
  , getEcdsaSecp256k1MessageByteArray
  , byteArrayToEcdsaSecp256k1SignatureUnsafe
  , ecdsaSecp256k1Signature
  , serialiseEcdsaSecp256k1PubKey
  , serialiseEcdsaSecp256k1PrivateKey
  , serialiseEcdsaSecp256k1SignatureToDer
  , serialiseEcdsaSecp256k1Signature
  , blake2b256Hash
  ) where

import Contract.Prelude

import Contract.PlutusData (class FromData, class ToData, fromData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray

-- | Invariant: length of the pubkey must be 33 bytes.
-- | Format: Compressed and serialized as per ECDSA signatures for SECP256k1.
-- | Check the leading byte for valid key format.
newtype EcdsaSecp256k1PubKey = EcdsaSecp256k1PubKey ByteArray

derive newtype instance Eq EcdsaSecp256k1PubKey

derive newtype instance Ord EcdsaSecp256k1PubKey

derive newtype instance ToData EcdsaSecp256k1PubKey

instance FromData EcdsaSecp256k1PubKey where
  fromData = fromData >=> ecdsaSecp256k1PubKey

derive instance Newtype EcdsaSecp256k1PubKey _

instance Show EcdsaSecp256k1PubKey where
  show (EcdsaSecp256k1PubKey byteArray) =
    "(byteArrayToEcdsaSecp256k1PubKeyUnsafe "
      <> show byteArray
      <> ")"

-- | Hex serializes the raw bytes
serialiseEcdsaSecp256k1PubKey :: EcdsaSecp256k1PubKey -> String
serialiseEcdsaSecp256k1PubKey (EcdsaSecp256k1PubKey pubKey) =
  ByteArray.byteArrayToHex pubKey

-- | Hex serializes the raw bytes
serialiseEcdsaSecp256k1PrivateKey :: EcdsaSecp256k1PrivateKey -> String
serialiseEcdsaSecp256k1PrivateKey (EcdsaSecp256k1PrivateKey privKey) =
  ByteArray.byteArrayToHex privKey

-- | Hex serializes the raw bytes
serialiseEcdsaSecp256k1Signature :: EcdsaSecp256k1Signature -> String
serialiseEcdsaSecp256k1Signature (EcdsaSecp256k1Signature privKey) =
  ByteArray.byteArrayToHex privKey

-- | Smart constructor for `EcdsaSecp256k1PubKey` to ensure it is a valid
-- | compressed (33 bytes) secp256k1 public key.
ecdsaSecp256k1PubKey :: ByteArray -> Maybe EcdsaSecp256k1PubKey
ecdsaSecp256k1PubKey bs
  | ByteArray.byteLength bs == 33
      && pubKeyVerify bs = Just $ EcdsaSecp256k1PubKey bs
  | otherwise = Nothing

-- | Get the underlying `ByteArray` of the `EcdsaSecp256k1PubKey`.
getEcdsaSecp256k1PubKeyByteArray :: EcdsaSecp256k1PubKey -> ByteArray
getEcdsaSecp256k1PubKeyByteArray (EcdsaSecp256k1PubKey byteArray) = byteArray

-- | Construct a `EcdsaSecp256k1PubKey` without verifying invariants. Use with
-- | extreme care.
byteArrayToEcdsaSecp256k1PubKeyUnsafe :: ByteArray -> EcdsaSecp256k1PubKey
byteArrayToEcdsaSecp256k1PubKeyUnsafe = EcdsaSecp256k1PubKey

-- | Invariant 1: length of the privkey must be 32 bytes
-- | Invariant 2: privkey must be nonzero
-- | Invariant 3: privkey must be less than the secp256k1 curve order (see
-- | reference for details)
-- |
-- | Reference: https://github.com/bitcoin-core/secp256k1/blob/e3f84777eba58ea010e61e02b0d3a65787bc4fd7/include/secp256k1.h#L662-L673
newtype EcdsaSecp256k1PrivateKey = EcdsaSecp256k1PrivateKey ByteArray

derive newtype instance Ord EcdsaSecp256k1PrivateKey

derive newtype instance Eq EcdsaSecp256k1PrivateKey

derive newtype instance ToData EcdsaSecp256k1PrivateKey

instance FromData EcdsaSecp256k1PrivateKey where
  fromData = fromData >=> ecdsaSecp256k1PrivateKey

instance Show EcdsaSecp256k1PrivateKey where
  show (EcdsaSecp256k1PrivateKey byteArray) =
    "(byteArrayToEcdsaSecp256k1PrivateKeyUnsafe "
      <> show byteArray
      <> ")"

-- | Smart constructor for `EcdsaSecp256k1PrivateKey` which checks its
-- | invariants.
ecdsaSecp256k1PrivateKey :: ByteArray -> Maybe EcdsaSecp256k1PrivateKey
ecdsaSecp256k1PrivateKey byteArray
  | ByteArray.byteLength byteArray == 32
      && secKeyVerify byteArray = Just $ EcdsaSecp256k1PrivateKey byteArray
  | otherwise = Nothing

-- | Construct an `EcdsaSecp256k1PrivateKey` without checking its invariants.
-- | Use with extreme care.
byteArrayToEcdsaSecp256k1PrivateKeyUnsafe ::
  ByteArray -> EcdsaSecp256k1PrivateKey
byteArrayToEcdsaSecp256k1PrivateKeyUnsafe = EcdsaSecp256k1PrivateKey

-- | Get the underlying `ByteArray` of an `EcdsaSecp256k1PrivateKey`.
getEcdsaSecp256k1PrivateKeyByteArray :: EcdsaSecp256k1PrivateKey -> ByteArray
getEcdsaSecp256k1PrivateKeyByteArray (EcdsaSecp256k1PrivateKey byteArray) =
  byteArray

-- | Invariant: length of the message must be 32 bytes.
newtype EcdsaSecp256k1Message = EcdsaSecp256k1Message ByteArray

derive newtype instance Ord EcdsaSecp256k1Message

derive newtype instance Eq EcdsaSecp256k1Message

derive newtype instance ToData EcdsaSecp256k1Message

instance FromData EcdsaSecp256k1Message where
  fromData = fromData >=> ecdsaSecp256k1Message

instance Show EcdsaSecp256k1Message where
  show (EcdsaSecp256k1Message byteArray) =
    "(byteArrayToEcdsaSecp256k1MessageUnsafe "
      <> show byteArray
      <> ")"

-- | Smart constructor for `EcdsaSecp256k1Message` which verifies its invariant.
ecdsaSecp256k1Message :: ByteArray -> Maybe EcdsaSecp256k1Message
ecdsaSecp256k1Message byteArray
  | ByteArray.byteLength byteArray == 32 = Just $ EcdsaSecp256k1Message byteArray
  | otherwise = Nothing

-- | Construct an `EcdsaSecp256k1Message` without verifying its invariant. Use
-- | with extreme care.
byteArrayToEcdsaSecp256k1MessageUnsafe :: ByteArray -> EcdsaSecp256k1Message
byteArrayToEcdsaSecp256k1MessageUnsafe = EcdsaSecp256k1Message

-- | Get the underlying `ByteArray` from an `EcdsaSecp256k1Message`.
getEcdsaSecp256k1MessageByteArray :: EcdsaSecp256k1Message -> ByteArray
getEcdsaSecp256k1MessageByteArray (EcdsaSecp256k1Message byteArray) = byteArray

-- | Invariant: length of the signature must be 64 bytes.
newtype EcdsaSecp256k1Signature = EcdsaSecp256k1Signature ByteArray

derive newtype instance Ord EcdsaSecp256k1Signature

derive newtype instance Eq EcdsaSecp256k1Signature

derive newtype instance ToData EcdsaSecp256k1Signature

instance FromData EcdsaSecp256k1Signature where
  fromData = fromData >=> ecdsaSecp256k1Signature

instance Show EcdsaSecp256k1Signature where
  show (EcdsaSecp256k1Signature byteArray) =
    "(byteArrayToEcdsaSecp256k1SignatureUnsafe "
      <> show byteArray
      <> ")"

-- | `ecdsaSecp256k1Signature` is a smart constructor for `EcdsaSecp256k1Signature` to
-- | verify the invariants.
ecdsaSecp256k1Signature :: ByteArray -> Maybe EcdsaSecp256k1Signature
ecdsaSecp256k1Signature byteArray
  | ByteArray.byteLength byteArray == 64 = Just $ EcdsaSecp256k1Signature
      byteArray
  | otherwise = Nothing

-- | Get the underlying `ByteArray` of a `EcdsaSecp256k1Signature`
getEcdsaSecp256k1SignatureByteArray :: EcdsaSecp256k1Signature -> ByteArray
getEcdsaSecp256k1SignatureByteArray (EcdsaSecp256k1Signature byteArray) =
  byteArray

-- | Construct a `EcdsaSecp256k1Signature` without verifying invariants. Use
-- | with extreme care.
byteArrayToEcdsaSecp256k1SignatureUnsafe :: ByteArray -> EcdsaSecp256k1Signature
byteArrayToEcdsaSecp256k1SignatureUnsafe = EcdsaSecp256k1Signature

-- TODO: newtype checks the type aliases above

foreign import generateRandomPrivateKey :: Effect EcdsaSecp256k1PrivateKey

foreign import toPubKeyUnsafe ::
  EcdsaSecp256k1PrivateKey -> EcdsaSecp256k1PubKey

foreign import pubKeyVerify :: ByteArray -> Boolean

foreign import secKeyVerify :: ByteArray -> Boolean

foreign import sign ::
  EcdsaSecp256k1Message -> EcdsaSecp256k1PrivateKey -> EcdsaSecp256k1Signature

foreign import signatureExport :: EcdsaSecp256k1Signature -> ByteArray

-- | Serialises a signature to DER format as hex encoded bytes
serialiseEcdsaSecp256k1SignatureToDer :: EcdsaSecp256k1Signature -> String
serialiseEcdsaSecp256k1SignatureToDer = ByteArray.byteArrayToHex <<<
  signatureExport

foreign import blake2b256Hash :: ByteArray -> ByteArray
