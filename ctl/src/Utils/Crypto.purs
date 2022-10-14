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
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Types.ByteArray (ByteArray)

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

-- | Invariant: ∀ x : Signature. length x = 71
-- | Format: DER encoding
type Signature = ByteArray

-- TODO: newtype checks the type aliases above

foreign import generateRandomPrivateKey ∷ Effect PrivateKey
foreign import toPubKeyUnsafe ∷ PrivateKey → PublicKey
foreign import sign ∷ Message → PrivateKey → Signature
foreign import verifyEcdsaSecp256k1Signature ∷
  PublicKey → Message → Signature → Boolean

multiSign ∷ Array PrivateKey → Message → Array Signature
multiSign xkeys msg = map (sign msg) xkeys

generatePrivKey ∷ Contract () ByteArray
generatePrivKey = liftEffect generateRandomPrivateKey
