-- | `Utils.Address` provides some utility functions for handling addresses.
module TrustlessSidechain.Utils.Address
  ( Bech32Bytes
  , getBech32BytesByteArray
  , bech32BytesFromAddress
  , addressFromBech32Bytes
  , byteArrayToBech32BytesUnsafe
  , getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  , toValidatorHash
  , toAddress
  , toCurrencySymbol
  , getCurrencyInfo
  , getCurrencySymbol
  , getCurrencySymbolHex
  , getValidatorHashHex
  , getValidatorHash
  , currencySymbolToHex
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash
  , getNetworkId
  , validatorHashEnterpriseAddress
  )
import Contract.Address as Address
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Monad as Monad
import Contract.PlutusData (class FromData, class ToData, PlutusData)
import Contract.Prim.ByteArray (ByteArray, CborBytes(CborBytes))
import Contract.Prim.ByteArray as ByteArray
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , ValidatorHash(ValidatorHash)
  , validatorHash
  )
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  )
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
import Ctl.Internal.Serialization.Hash (scriptHashToBytes)
import Data.Array as Array
import TrustlessSidechain.Error
  ( OffchainError
      ( NotFoundOwnPubKeyHash
      , ConversionError
      , NotFoundOwnAddress
      , InvalidCurrencySymbol
      , InvalidScript
      , InvalidAddress
      )
  )
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.Types (ScriptId)

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

-- | Return a single own payment pub key hash without generating warnings.
getOwnPaymentPubKeyHash ∷
  Contract PaymentPubKeyHash
getOwnPaymentPubKeyHash =
  liftedM (show NotFoundOwnPubKeyHash)
    (ownPaymentPubKeyHashes >>= pure <<< Array.head)

-- | Return a single own wallet address without generating warnings.
getOwnWalletAddress ∷
  Contract Address
getOwnWalletAddress =
  liftedM (show NotFoundOwnAddress)
    (getWalletAddresses >>= pure <<< Array.head)

-- | Convert Address to ValidatorHash, raising an error if an address does not
-- | represent a script.
toValidatorHash ∷ Address → Contract ValidatorHash
toValidatorHash addr =
  liftContractM
    (show $ InvalidAddress "Cannot convert Address to ValidatorHash" addr)
    (Address.toValidatorHash addr)

-- | Convert ValidatorHash to Address in the current network, raising an error
-- | if the hash is not valid.
toAddress ∷ ValidatorHash → Contract Address
toAddress vh = do
  netId ← getNetworkId
  liftContractM
    ( show $ ConversionError $ "Cannot convert validator hash " <> show vh
        <> " to an enterprise address"
    )
    (validatorHashEnterpriseAddress netId vh)

-- | `getCurrencyInfo` returns minting policy and currency symbol of a given
-- | script.  Requires providing parameters of that script.
getCurrencyInfo ∷
  ScriptId →
  Array PlutusData →
  Contract CurrencyInfo
getCurrencyInfo scriptId params = do
  mintingPolicy ← mkMintingPolicyWithParams scriptId params
  currencySymbol ← getCurrencySymbol scriptId mintingPolicy
  pure $ { mintingPolicy, currencySymbol }

-- | `getCurrencySymbolHex` converts a minting policy with known script IDto its
-- | currency symbol
getCurrencySymbol ∷ ScriptId → MintingPolicy → Contract CurrencySymbol
getCurrencySymbol scriptId mp = do
  Monad.liftContractM (show $ InvalidCurrencySymbol scriptId mp) $
    Value.scriptCurrencySymbol mp

-- | `getCurrencySymbolHex` converts a minting policy with unknown script ID to
-- | its currency symbol
toCurrencySymbol ∷ MintingPolicy → Contract CurrencySymbol
toCurrencySymbol mp = do
  Monad.liftContractM (show $ InvalidScript (show mp)) $
    Value.scriptCurrencySymbol mp

-- | `getCurrencySymbolHex` converts a minting policy to its hex encoded
-- | currency symbol
getCurrencySymbolHex ∷ ScriptId → MintingPolicy → Contract String
getCurrencySymbolHex name mp =
  currencySymbolToHex <$> getCurrencySymbol name mp

-- | `getValidatorHashHex` converts a validator hash to a hex encoded string
getValidatorHashHex ∷ ValidatorHash → String
getValidatorHashHex (ValidatorHash sh) =
  ByteArray.byteArrayToHex $ unwrap $ scriptHashToBytes sh

-- | `getValidatorHash` converts a validator to a hex encoded string
getValidatorHash ∷ Validator → String
getValidatorHash = getValidatorHashHex <<< validatorHash

-- | Convert a currency symbol to hex encoded string
currencySymbolToHex ∷ CurrencySymbol → String
currencySymbolToHex =
  ByteArray.byteArrayToHex <<< Value.getCurrencySymbol
