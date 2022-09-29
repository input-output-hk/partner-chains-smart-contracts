module Utils.Crypto
  ( toPubKeyUnsafe
  , sign
  , verifyEd25519Signature
  , generatePrivKey
  , multiSign
  , hexToPrivKeyUnsafe
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayToIntArray)
import Serialization.Types (PrivateKey, PublicKey)
import Types (PubKey, Signature)
import Types.ByteArray (ByteArray, hexToByteArrayUnsafe)

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
