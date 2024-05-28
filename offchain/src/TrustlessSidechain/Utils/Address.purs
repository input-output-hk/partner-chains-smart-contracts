-- | `Utils.Address` provides some utility functions for handling addresses.
module TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  , toScriptHash
  , toAddress
  , getCurrencyInfo
  , getScriptHashHex
  , getPlutusScriptHex
  , fromPaymentPubKeyHash
  , addressFromBech32Bytes
  ) where


import Cardano.AsCbor (decodeCbor)
import Data.BigInt as BigInt
import Data.Unfoldable (unfoldr)
import Data.String.CodeUnits (singleton)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Data.String.CodePoints (drop, take, length) as String


import Cardano.Types.PaymentCredential (PaymentCredential(PaymentCredential))
import Cardano.Types.NetworkId (NetworkId(MainnetId, TestnetId))
import Cardano.Serialization.Lib as Csl
import Contract.Prelude hiding (note)
import Cardano.Serialization.Lib
  ( fromBytes
  , toBytes
  )
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash(PaymentPubKeyHash))
import Cardano.Types.Credential (Credential(PubKeyHashCredential, ScriptHashCredential))
import Contract.Address as Address
import Cardano.Types.Address (toBech32, fromBech32) as Address
import Contract.PlutusData (class FromData, class ToData, PlutusData)
import Contract.Prim.ByteArray (ByteArray, CborBytes(CborBytes))
import Contract.Prim.ByteArray as ByteArray
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptHash (fromBech32, toBech32Unsafe) as ScriptHash
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Control.Alternative ((<|>))
import Cardano.Types.Address (Address)
import Cardano.Types.Address as Address
import Partial.Unsafe (unsafePartial)
import Data.Array as Array
import Run (Run)
import Run.Except (EXCEPT, note)
import Run.Except as Run
import TrustlessSidechain.Effects.Wallet
  ( WALLET
  , getNetworkId
  , getWalletAddresses
  )
import TrustlessSidechain.Effects.Wallet as Effect
import TrustlessSidechain.Error
  ( OffchainError
      ( NotFoundOwnPubKeyHash
      , InvalidAddress
      , ConversionError
      , InvalidCurrencySymbol
      , InvalidScript
      )
  )
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.Types (ScriptId)
import Type.Row (type (+))

-- | Return a single own payment pub key hash without generating warnings.
getOwnPaymentPubKeyHash ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r) PaymentPubKeyHash
getOwnPaymentPubKeyHash = do
  pubKeyHashes ← Effect.ownPaymentPubKeyHashes
  Run.note NotFoundOwnPubKeyHash $ Array.head pubKeyHashes

-- | Return a single own wallet address without generating warnings.
getOwnWalletAddress ∷
  ∀ r.
  Run (EXCEPT OffchainError + WALLET + r) Address
getOwnWalletAddress = do
  addresses ← getWalletAddresses
  Run.note NotFoundOwnPubKeyHash $ Array.head addresses

-- | Convert Address to ValidatorHash, raising an error if an address does not
-- | represent a script.
toScriptHash ∷ ∀ r. Address → Run (EXCEPT OffchainError + r) ScriptHash
toScriptHash addr = do
  PaymentCredential c <- note
    (InvalidAddress "Cannot convert Address to ScriptHash" addr)
    $ Address.getPaymentCredential addr

  case c of
    ScriptHashCredential sh -> pure sh
    _ -> Run.throw $ InvalidAddress "Address does not represent a script" addr

-- | Convert ValidatorHash to Address in the current network, raising an error
-- | if the hash is not valid.
toAddress ∷ ∀ r. ScriptHash → Run (EXCEPT OffchainError + WALLET + r) Address
toAddress sh = do
  netId ← getNetworkId
  pure $
    Address.mkPaymentAddress netId (wrap $ ScriptHashCredential sh) Nothing

-- | `getCurrencyInfo` returns minting policy and currency symbol of a given
-- | script.  Requires providing parameters of that script.
getCurrencyInfo ∷
  ∀ r.
  ScriptId →
  Array PlutusData →
  Run (EXCEPT OffchainError + r) CurrencyInfo
getCurrencyInfo scriptId params = do
  plutusScript ← mkMintingPolicyWithParams scriptId params

  pure $ { mintingPolicy: plutusScript
         , currencySymbol: PlutusScript.hash plutusScript
         }

getPlutusScriptHex :: Partial => PlutusScript -> String
getPlutusScriptHex = ByteArray.byteArrayToHex <<< toBytes <<< PlutusScript.toCsl

getScriptHashHex :: ScriptHash -> String
getScriptHashHex = ByteArray.byteArrayToHex <<< toBytes <<< unwrap

fromPaymentPubKeyHash :: NetworkId -> PaymentPubKeyHash -> Address
fromPaymentPubKeyHash networkId pkh = Address.mkPaymentAddress networkId (wrap $ PubKeyHashCredential $ unwrap pkh) Nothing





hexToAsciiString :: String -> String
hexToAsciiString hexStr =
  let
    hexPairs :: Array String
    hexPairs = unfoldr nextPair hexStr

    nextPair :: String -> Maybe (String /\ String)
    nextPair str =
      if String.length str < 2 then Nothing
      else Just (String.take 2 str /\ String.drop 2 str)

    bytes :: Array BigInt.BigInt
    bytes = map (fromMaybe (BigInt.fromInt 0) <<< BigInt.fromBase 2) hexPairs

    charArray :: Array String
    charArray = map (singleton <<< toChar) bytes
  in
    fold charArray
  where
    toChar :: BigInt.BigInt -> Char
    toChar x = unsafePartial $ fromJust (toEnum =<< BigInt.toInt x)


addressFromBech32Bytes :: ByteArray -> Maybe Address
addressFromBech32Bytes = decodeCbor <<< wrap


