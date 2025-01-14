module TrustlessSidechain.Options.Parsers
  ( assetNameParser
  , bech32AddressParser
  , bech32BytesParser
  , bech32ValidatorHashParser
  , bigInt
  , blockHash
  , byteArray
  , cborEncodedAddressParser
  , currencySymbolParser
  , depositAmount
  , epoch
  , governanceAuthority
  , mkCurrencySymbol
  , networkId
  , parseAssetName
  , parsePubKeyBytesAndSignatureBytes
  , parseTokenName
  , permissionedCandidateKeys
  , permissionedCandidatesCount
  , plutusDataParser
  , positiveAmount
  , posixTime
  , pubKeyBytesAndSignatureBytes
  , pubKeyHash
  , registeredCandidatesCount
  , registrationSidechainKeys
  , sidechainAddress
  , tokenAmount
  , transactionInput
  , uint
  , validatorHashParser
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.FromData (fromData)
import Cardano.Plutus.Types.Address (fromCardano)
import Cardano.Plutus.Types.Credential (Credential(ScriptCredential))
import Cardano.Serialization.Lib (toBytes)
import Cardano.Types.Address (Address, fromBech32, toCsl)
import Cardano.Types.AssetName as AssetName
import Cardano.Types.BigNum (BigNum, fromBigInt)
import Cardano.Types.NetworkId (NetworkId(MainnetId, TestnetId))
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash(PaymentPubKeyHash))
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.CborBytes (CborBytes, cborBytesFromByteArray, hexToCborBytes)
import Contract.PlutusData (class FromData)
import Contract.Transaction (TransactionInput(TransactionInput))
import Contract.Value (AssetName, CurrencySymbol, TokenName)
import Ctl.Internal.Types.Interval (POSIXTime(..))
import Data.ByteArray (ByteArray)
import Data.ByteArray as ByteArray
import Data.Either as Either
import Data.Maybe (fromJust, isJust)
import Data.String (Pattern(Pattern), split, stripPrefix)
import Data.UInt (UInt)
import Data.UInt as UInt
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Options.Applicative (ReadM, eitherReader, maybeReader, readerError)
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Governance.Admin as Governance

hexToByteArray :: String -> Maybe ByteArray
hexToByteArray s = ByteArray.hexToByteArray $ fromMaybe s $ stripPrefix
  (Pattern "0x")
  s

-- | Parses a hex encoded cbor of plutus data
parsePlutusData :: forall a. FromData a => String -> Either String a
parsePlutusData str = case hexToCborBytes str of
  Nothing -> Left "invalid hex string"
  Just cborBytes -> case fromData =<< decodeCbor cborBytes of
    Nothing -> Left "invalid cbor"
    Just x -> Right x

-- | Wraps `parsePlutusData`
plutusDataParser :: forall a. FromData a => ReadM a
plutusDataParser = eitherReader parsePlutusData

-- | Parse a transaction input from a CLI format (e.g. `aabbcc#0`)
transactionInput :: ReadM TransactionInput
transactionInput = maybeReader \txIn ->
  case split (Pattern "#") txIn of
    [ txId, txIdx ] -> ado
      index <- UInt.fromString txIdx
      transactionId <- (decodeCbor <<< wrap) =<< hexToByteArray txId
      in
        TransactionInput
          { transactionId
          , index
          }
    _ -> Nothing

cborEncodedAddressParser :: ReadM Address
cborEncodedAddressParser = cbor >>= decodeCbor >>>
  maybe (readerError "Error while parsing supplied CBOR as Address.")
    pure

validatorHashParser :: ReadM ScriptHash
validatorHashParser = do
  ba <- byteArray
  sh <- case decodeCbor $ wrap ba of
    Just vh' -> pure vh'
    Nothing -> readerError "script hash deserialization failed."

  pure $ sh

-- | `bech32AddressParser` parses a human readable bech32 address into an
-- | address
-- TODO: this does *not* check if the network of the address coincides with the
-- network that CTL is running on.
parseHumanReadableBech32ToAddress :: String -> Either String Address
parseHumanReadableBech32ToAddress str = do
  addr <- case (fromBech32 str) of
    Just x -> Right x
    Nothing -> Left "bech32 address deserialization failed."

  pure addr

-- | `parseHumanReadableBech32ToValidatorHash` parses a human readable bech32
-- address into validator hash
parseHumanReadableBech32ToValidatorHash :: String -> Either String ScriptHash
parseHumanReadableBech32ToValidatorHash str = do
  addr <- case (fromBech32 str) of
    Just x -> Right x
    Nothing -> Left "bech32 address deserialization failed."

  addr' <- case fromCardano addr of
    Just x -> Right x
    Nothing -> Left "bech32 address conversion to plutus address failed"

  vh <- case (unwrap addr').addressCredential of
    ScriptCredential vh' -> Right vh'
    _ -> Left "provided address is not a script address"
  pure $ unwrap vh

-- | parses a human readable bech32 address to the bech32 bytes.
parseHumanReadableBech32ToBech32Bytes :: String -> Either String ByteArray
parseHumanReadableBech32ToBech32Bytes str = do
  addr <- parseHumanReadableBech32ToAddress str
  pure $ toBytes $ toCsl addr

-- | Wraps `parseHumanReadableBech32ToAddress`
bech32AddressParser :: ReadM Address
bech32AddressParser = eitherReader parseHumanReadableBech32ToAddress

-- | Wraps `parseHumanReadableBech32ToValidatorHash`
bech32ValidatorHashParser :: ReadM ScriptHash
bech32ValidatorHashParser = eitherReader parseHumanReadableBech32ToValidatorHash

-- | Wraps `parseHumanReadableBech32ToBech32Bytes  `
bech32BytesParser :: ReadM ByteArray
bech32BytesParser = eitherReader parseHumanReadableBech32ToBech32Bytes

-- | Parse ByteArray from hexadecimal representation
byteArray :: ReadM ByteArray
byteArray = maybeReader hexToByteArray

-- | Parses a PubKeyHash from hexadecimal representation.
pubKeyHash :: ReadM PaymentPubKeyHash
pubKeyHash = maybeReader
  $ hexToByteArray
  >=> (wrap >>> decodeCbor)
  >=> PaymentPubKeyHash
  >>> pure

-- | Parses a GovernanceAuthority from hexadecimal representation.
governanceAuthority :: ReadM Governance.GovernanceAuthority
governanceAuthority =
  Governance.mkGovernanceAuthority <$> pubKeyHash

networkId :: ReadM NetworkId
networkId = eitherReader
  ( \str -> case str of
      "mainnet" -> pure MainnetId
      "testnet" -> pure TestnetId
      _ -> Left "network-id must be either 'mainnet' or 'testnet'"
  )

-- | Parse only CBOR encoded hexadecimal
-- Note: This assumes there will be some validation with the CborBytes, otherwise
-- we should simplify the code and fall back to ByteArray.
cbor :: ReadM CborBytes
cbor = cborBytesFromByteArray <$> byteArray

-- | Parse BigInt
bigInt :: ReadM BigInt
bigInt = maybeReader BigInt.fromString

-- | Parse a permissioned candidates count
permissionedCandidatesCount :: ReadM BigInt
permissionedCandidatesCount = eitherReader
  ( \str -> case BigInt.fromString str of
      Just i
        | i >= zero -> pure i
        | otherwise -> Left "permissioned-candidates-count must be non-negative"
      Nothing -> Left "failed to parse int permissioned-candidates-count"
  )

-- | Parse a registered candidates count
registeredCandidatesCount :: ReadM BigInt
registeredCandidatesCount = eitherReader
  ( \str -> case BigInt.fromString str of
      Just i
        | i >= zero -> pure i
        | otherwise -> Left "registered-candidates-count must be non-negative"
      Nothing -> Left "failed to parse int registered-candidates-count"
  )

-- | Parse an epoch.
epoch :: ReadM BigInt
epoch = eitherReader
  ( \str -> case BigInt.fromString str of
      Just i
        | i >= zero -> pure i
        | otherwise -> Left "epoch must be non negative"
      Nothing -> Left "failed to parse int epoch"
  )

-- | Parse UInt
uint :: ReadM UInt
uint = maybeReader UInt.fromString

sidechainAddress :: ReadM ByteArray
sidechainAddress = hexString

blockHash :: ReadM ByteArray
blockHash = hexString

-- | ```
-- | hexString
-- |        -> 0x hexStr
-- |        -> hexStr
-- | ```
-- where `hexStr` is a sequence of hex digits.
hexString :: ReadM ByteArray
hexString = maybeReader $ \str ->
  case split (Pattern "0x") str of
    [ "", hex ] -> hexToByteArray hex
    [ hex ] -> hexToByteArray hex
    _ -> Nothing

-- | `pubKeyBytesAndSignatureBytes` is a the CLI parser for `parsePubKeyBytesAndSignatureBytes`.
pubKeyBytesAndSignatureBytes :: ReadM (ByteArray /\ Maybe ByteArray)
pubKeyBytesAndSignatureBytes = maybeReader parsePubKeyBytesAndSignatureBytes

-- | `parsePubKeyBytesAndSignatureBytes` parses the following format
-- | `hexStr[:[hexStr]]`
-- Note: should we make this more strict and disallow `aa:`? in a sense:
-- `aa` denotes a pubkey without a signature
-- `aa:bb` denotes a pubkey and a signature
-- anything else is likely an error, and should be treated as malformed input
parsePubKeyBytesAndSignatureBytes ::
  String -> Maybe (ByteArray /\ Maybe ByteArray)
parsePubKeyBytesAndSignatureBytes str =
  case split (Pattern ":") str of
    [ l, r ] | l /= "" -> do
      l' <- hexToByteArray $ l
      if r == "" then pure $ l' /\ Nothing
      else do
        r' <- hexToByteArray $ r
        pure $ l' /\ Just r'
    [ l ] -> ado
      l' <- hexToByteArray $ l
      in l' /\ Nothing
    _ -> Nothing

registrationSidechainKeys ::
  ReadM
    { sidechainKey :: ByteArray
    , auraKey :: ByteArray
    , grandpaKey :: ByteArray
    }
registrationSidechainKeys = eitherReader parseRegistrationSidechainKeys

parseRegistrationSidechainKeys ::
  String ->
  Either String
    { sidechainKey :: ByteArray
    , auraKey :: ByteArray
    , grandpaKey :: ByteArray
    }
parseRegistrationSidechainKeys str =
  case split (Pattern ":") str of
    [ sidechainKey', auraKey', grandpaKey' ] -> do
      sidechainKey <- Either.note ("sidechainKey must be a valid hex string") $
        hexToByteArray sidechainKey'
      auraKey <-
        Either.note ("auraKey must be a valid hex string") $
          hexToByteArray auraKey'
      grandpaKey <- Either.note ("grandpaKey must be a valid hex string") $
        hexToByteArray grandpaKey'
      pure { sidechainKey, auraKey, grandpaKey }
    _ -> Left
      "sidechain-keys must be a 3 hex strings concatenated with colons, for example: aa:bb:cc"

permissionedCandidateKeys ::
  ReadM
    { sidechainKey :: ByteArray
    , auraKey :: ByteArray
    , grandpaKey :: ByteArray
    }
permissionedCandidateKeys = eitherReader parsePermissionedCandidateKeys

parsePermissionedCandidateKeys ::
  String ->
  Either String
    { sidechainKey :: ByteArray
    , auraKey :: ByteArray
    , grandpaKey :: ByteArray
    }
parsePermissionedCandidateKeys str =
  case split (Pattern ":") str of
    [ sidechainKey', auraKey', grandpaKey' ] -> do
      sidechainKey <- Either.note ("sidechainKey must be a valid hex string") $
        hexToByteArray sidechainKey'
      auraKey <-
        Either.note ("auraKey must be a valid hex string") $
          hexToByteArray auraKey'
      grandpaKey <- Either.note ("grandpaKey must be a valid hex string") $
        hexToByteArray grandpaKey'
      pure $
        { sidechainKey
        , auraKey
        , grandpaKey
        }
    _ -> Left
      "permissioned-candidate-keys must be 3 hex strings concatenated with colons, for example: aa:bb:cc"

-- | `parseAssetName` is a thin wrapper around `Cardano.Types.AssetName.mkAssetName` for
-- | converting hex encoded strings to token names
parseAssetName :: String -> Maybe (AssetName)
parseAssetName hexStr = do
  ba <- hexToByteArray hexStr
  AssetName.mkAssetName ba

-- | `assetName` wraps `parseAssetName`.
assetNameParser :: ReadM AssetName
assetNameParser = maybeReader parseAssetName

-- | `parseTokenName` is a thin wrapper around `Contract.Value.mkTokenName` for
-- | converting hex encoded strings to token names
parseTokenName :: String -> Maybe (TokenName)
parseTokenName = parseAssetName

-- | `parseCurrencySymbol` is a thin wrapper around `Contract.Value.mkCurrencySymbol` for
-- | converting hex encoded strings to token names
currencySymbolParser :: ReadM CurrencySymbol
currencySymbolParser = validatorHashParser

positiveAmount :: String -> String -> ReadM BigInt
positiveAmount parseError negativeNumberError = eitherReader
  ( \str -> case BigInt.fromString str of
      Just i
        | i >= zero -> pure i
        | otherwise -> Left negativeNumberError
      Nothing -> Left parseError
  )

tokenAmount :: ReadM BigNum
tokenAmount = eitherReader
  ( \str -> case BigInt.fromString str of
      Just i
        | i >= zero && isJust (fromBigInt i) -> pure
            $ unsafePartial
            $ fromJust
            $ fromBigInt i
        | otherwise -> Left "token-amount amount must be non-negative"
      Nothing -> Left "failed to parse token-amount"
  )

depositAmount :: ReadM BigInt.BigInt
depositAmount = eitherReader
  ( \str -> case BigInt.fromString str of
      Just i
        | i >= zero -> pure i
        | otherwise -> Left "deposit-amount amount must be non-negative"
      Nothing -> Left "failed to parse deposit-amount"
  )

posixTime :: ReadM POSIXTime
posixTime = POSIXTime <$> bigInt

mkCurrencySymbol :: String -> Maybe CurrencySymbol
mkCurrencySymbol s = decodeCbor =<< hexToCborBytes s
