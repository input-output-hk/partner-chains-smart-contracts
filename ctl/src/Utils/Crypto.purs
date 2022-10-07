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
type PublicKey = ByteArray

-- | Invariant: ∀ x : PrivateKey. length x = 32
type PrivateKey = ByteArray

-- | Invariant: ∀ x : Message. length x = 32
type Message = ByteArray

-- | Invariant: ∀ x : Signature. length x = 64
type Signature = ByteArray

foreign import generateRandomPrivateKey ∷ Effect PrivateKey
foreign import toPubKeyUnsafe ∷ PrivateKey → PublicKey
foreign import sign ∷ PrivateKey → Message → Signature
foreign import verifyEcdsaSecp256k1Signature ∷
  PublicKey → Message → Signature → Boolean

multiSign ∷ Array PublicKey → Message → Array Signature
multiSign xkeys msg = map (flip sign msg) xkeys

generatePrivKey ∷ Contract () ByteArray
generatePrivKey = liftEffect generateRandomPrivateKey
