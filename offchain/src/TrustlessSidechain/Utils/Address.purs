module TrustlessSidechain.Utils.Address
  ( Bech32Bytes
  , getBech32BytesByteArray
  , bech32BytesFromAddress
  , addressFromBech32Bytes
  , byteArrayToBech32BytesUnsafe
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData (class FromData, class ToData)
import Contract.Prim.ByteArray (ByteArray, CborBytes(..))
import Control.Alternative ((<|>))
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress, toPlutusAddress)
import Ctl.Internal.Serialization.Address
  ( baseAddressBytes
  , baseAddressFromAddress
  , baseAddressFromBytes
  , baseAddressToAddress
  , enterpriseAddressBytes
  , enterpriseAddressFromAddress
  , enterpriseAddressFromBytes
  , enterpriseAddressToAddress
  , intToNetworkId
  , pointerAddressBytes
  , pointerAddressFromAddress
  , pointerAddressFromBytes
  , pointerAddressToAddress
  , rewardAddressBytes
  , rewardAddressFromAddress
  , rewardAddressFromBytes
  , rewardAddressToAddress
  )

-- | `Bech32Bytes` is a newtype wrapper for bech32 encoded bytestrings. In
-- | particular, this is used in the `recipient` field of `MerkleTreeEntry`
-- | which should be a decoded bech32 cardano address.
-- | See [here](https://cips.cardano.org/cips/cip19/) for details.
newtype Bech32Bytes = Bech32Bytes ByteArray

-- | `getBech32BytesByteArray` gets the underlying `ByteArray` of `Bech32Bytes`
getBech32BytesByteArray ∷ Bech32Bytes → ByteArray
getBech32BytesByteArray (Bech32Bytes byteArray) = byteArray

derive newtype instance ordBech32Bytes ∷ Ord Bech32Bytes
derive newtype instance eqBech32Bytes ∷ Eq Bech32Bytes
derive newtype instance toDataBech32Bytes ∷ ToData Bech32Bytes
derive newtype instance fromDataBech32Bytes ∷ FromData Bech32Bytes

instance Show Bech32Bytes where
  show (Bech32Bytes byteArray) = "(byteArrayToBech32BytesUnsafe "
    <> show byteArray
    <> ")"

-- | `byteArrayToBech32BytesUnsafe` converts a `ByteArray` to `Bech32Bytes`
-- | without checking the data format.
byteArrayToBech32BytesUnsafe ∷ ByteArray → Bech32Bytes
byteArrayToBech32BytesUnsafe = Bech32Bytes

-- | `bech32BytesFromAddress` serialises an `Address` to `Bech32Bytes` using
-- | the network id in the `Contract`
bech32BytesFromAddress ∷ Address → Maybe Bech32Bytes
bech32BytesFromAddress address = do
  netId ← intToNetworkId 0
  -- ^ Network ID is not going to be encoded into the byte representation,
  -- so this has no effect
  let cslAddr = fromPlutusAddress netId address

  bytes ←
    (baseAddressBytes <$> baseAddressFromAddress cslAddr)
      <|> (enterpriseAddressBytes <$> enterpriseAddressFromAddress cslAddr)
      <|> (pointerAddressBytes <$> pointerAddressFromAddress cslAddr)
      <|> (rewardAddressBytes <$> rewardAddressFromAddress cslAddr)

  pure $ Bech32Bytes $ unwrap bytes

-- | `addressFromBech32Bytes` is a convenient wrapper to convert cbor bytes
-- | into an `Address.`
-- | It is useful to use this with `Contract.CborBytes.cborBytesFromByteArray`
-- | to create an address from a `ByteArray` i.e.,
-- | ```
-- | addressFromBech32Bytes <<< Contract.CborBytes.cborBytesFromByteArray
-- | ```
-- | Then, you can use `bech32BytesFromAddress` to get the `recipient`.
addressFromBech32Bytes ∷ Bech32Bytes → Maybe Address
addressFromBech32Bytes bechBytes = do
  let cborBytes = CborBytes $ getBech32BytesByteArray bechBytes
  enterpriseAddr ←
    (baseAddressToAddress <$> baseAddressFromBytes cborBytes)
      <|> (enterpriseAddressToAddress <$> enterpriseAddressFromBytes cborBytes)
      <|> (pointerAddressToAddress <$> pointerAddressFromBytes cborBytes)
      <|> (rewardAddressToAddress <$> rewardAddressFromBytes cborBytes)

  toPlutusAddress enterpriseAddr
