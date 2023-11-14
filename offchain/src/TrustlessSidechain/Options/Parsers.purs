module TrustlessSidechain.Options.Parsers
  ( atmsKind
  , bech32AddressParser
  , bech32BytesParser
  , bech32ValidatorHashParser
  , bigInt
  , blockHash
  , byteArray
  , cborEncodedAddressParser
  , combinedMerkleProofParser
  , combinedMerkleProofParserWithPkh
  , committeeSignature
  , denominator
  , ecdsaSecp256k1PrivateKey
  , ecdsaSecp256k1PublicKey
  , epoch
  , governanceAuthority
  , numerator
  , parseATMSKind
  , parsePubKeyAndSignature
  , parsePubKeyBytesAndSignatureBytes
  , parseTokenName
  , permissionedCandidateKeys
  , permissionedCandidatesCount
  , plutusDataParser
  , pubKeyBytesAndSignatureBytes
  , pubKeyHash
  , registeredCandidatesCount
  , rootHash
  , schnorrSecp256k1PrivateKey
  , sidechainAddress
  , sidechainSignature
  , tokenName
  , transactionInput
  , uint
  , validatorHashParser
  ) where

import Contract.Prelude

import Contract.Address (Address, PubKeyHash(PubKeyHash))
import Contract.CborBytes (CborBytes, cborBytesFromByteArray, hexToCborBytes)
import Contract.PlutusData (class FromData)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray, hexToByteArray)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Contract.Value (TokenName)
import Contract.Value as Value
import Ctl.Internal.Plutus.Conversion.Address as Conversion.Address
import Ctl.Internal.Plutus.Types.Credential
  ( Credential(ScriptCredential)
  )
import Ctl.Internal.Serialization.Address as Serialization.Address
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashFromBytes
  , scriptHashFromBytes
  )
import Ctl.Internal.Types.Scripts (ValidatorHash(ValidatorHash))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either as Either
import Data.String (Pattern(Pattern), split)
import Data.UInt (UInt)
import Data.UInt as UInt
import Options.Applicative (ReadM, eitherReader, maybeReader, readerError)
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds
      ( ATMSPlainEcdsaSecp256k1
      , ATMSPlainSchnorrSecp256k1
      , ATMSDummy
      , ATMSPoK
      , ATMSMultisignature
      )
  )
import TrustlessSidechain.FUELMintingPolicy.V1 (CombinedMerkleProof)
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.Utils.Address
  ( Bech32Bytes
  , addressFromBech32Bytes
  , bech32BytesFromAddress
  )
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PrivateKey
  , EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SchnorrSecp256k1
  ( SchnorrSecp256k1PrivateKey
  )
import TrustlessSidechain.Utils.SchnorrSecp256k1 as Utils.SchnorrSecp256k1

-- | Wraps `parseATMSKind`
atmsKind ∷ ReadM ATMSKinds
atmsKind = eitherReader parseATMSKind

-- | Parses one of the possible kinds of committee certificate verifications
parseATMSKind ∷ String → Either String ATMSKinds
parseATMSKind str = case str of
  "plain-ecdsa-secp256k1" → Right
    ATMSPlainEcdsaSecp256k1
  "plain-schnorr-secp256k1" → Right
    ATMSPlainSchnorrSecp256k1
  "pok" → Right ATMSPoK
  "dummy" → Right ATMSDummy
  "multisignature" → Right ATMSMultisignature
  _ → Left
    "invalid ATMS kind expected either 'plain-ecdsa-secp256k1', 'plain-schnorr-secp256k1', 'multisignature', 'pok', or 'dummy'"

-- | Parses a hex encoded cbor of plutus data
parsePlutusData ∷ ∀ a. FromData a ⇒ String → Either String a
parsePlutusData str = case hexToCborBytes str of
  Nothing → Left "invalid hex string"
  Just cborBytes → case PlutusData.deserializeData cborBytes of
    Nothing → Left "invalid cbor"
    Just x → Right x

-- | Wraps `parsePlutusData`
plutusDataParser ∷ ∀ a. FromData a ⇒ ReadM a
plutusDataParser = eitherReader parsePlutusData

-- | Parse a transaction input from a CLI format (e.g. `aabbcc#0`)
transactionInput ∷ ReadM TransactionInput
transactionInput = maybeReader \txIn →
  case split (Pattern "#") txIn of
    [ txId, txIdx ] → ado
      index ← UInt.fromString txIdx
      transactionId ← TransactionHash <$> hexToByteArray txId
      in
        TransactionInput
          { transactionId
          , index
          }
    _ → Nothing

cborEncodedAddressParser ∷ ReadM Address
cborEncodedAddressParser = cbor >>= PlutusData.deserializeData >>>
  maybe (readerError "Error while parsing supplied CBOR as Address.")
    pure

validatorHashParser ∷ ReadM ValidatorHash
validatorHashParser = do
  ba ← byteArray
  vh ← case scriptHashFromBytes ba of
    Just vh' → pure vh'
    Nothing → readerError "script hash deserialization failed."

  pure $ ValidatorHash vh

-- | `bech32AddressParser` parses a human readable bech32 address into an
-- | address
-- TODO: this does *not* check if the network of the address coincides with the
-- network that CTL is running on.
parseHumanReadableBech32ToAddress ∷ String → Either String Address
parseHumanReadableBech32ToAddress str = do
  addr ← case (Serialization.Address.addressFromBech32 str) of
    Just x → Right x
    Nothing → Left "bech32 address deserialization failed."

  addr' ← case Conversion.Address.toPlutusAddress addr of
    Just x → Right x
    Nothing → Left "bech32 address conversion to plutus address failed"

  pure addr'

-- | `parseHumanReadableBech32ToValidatorHash` parses a human readable bech32
-- address into validator hash
parseHumanReadableBech32ToValidatorHash ∷ String → Either String ValidatorHash
parseHumanReadableBech32ToValidatorHash str = do
  addr ← case (Serialization.Address.addressFromBech32 str) of
    Just x → Right x
    Nothing → Left "bech32 address deserialization failed."

  addr' ← case Conversion.Address.toPlutusAddress addr of
    Just x → Right x
    Nothing → Left "bech32 address conversion to plutus address failed"

  vh ← case (unwrap addr').addressCredential of
    ScriptCredential vh' → Right vh'
    _ → Left "provided address is not a script address"
  pure vh

-- | parses a human readable bech32 address to the bech32 bytes.
parseHumanReadableBech32ToBech32Bytes ∷ String → Either String Bech32Bytes
parseHumanReadableBech32ToBech32Bytes str = do
  addr ← parseHumanReadableBech32ToAddress str
  case bech32BytesFromAddress addr of
    Nothing → Left "failed to get bech32 bytes from address"
    Just x → pure x

-- | Wraps `parseHumanReadableBech32ToAddress`
bech32AddressParser ∷ ReadM Address
bech32AddressParser = eitherReader parseHumanReadableBech32ToAddress

-- | Wraps `parseHumanReadableBech32ToValidatorHash`
bech32ValidatorHashParser ∷ ReadM ValidatorHash
bech32ValidatorHashParser = eitherReader parseHumanReadableBech32ToValidatorHash

-- | Wraps `parseHumanReadableBech32ToBech32Bytes  `
bech32BytesParser ∷ ReadM Bech32Bytes
bech32BytesParser = eitherReader parseHumanReadableBech32ToBech32Bytes

combinedMerkleProofParser ∷ ReadM CombinedMerkleProof
combinedMerkleProofParser = cbor >>= PlutusData.deserializeData >>>
  maybe (readerError "Error while parsing supplied CBOR as CombinedMerkleProof.")
    pure

-- | This parser will convert the raw bytestring to a valid Cardano address
combinedMerkleProofParserWithPkh ∷
  ReadM (CombinedMerkleProof /\ Address)
combinedMerkleProofParserWithPkh = do
  cmp ← combinedMerkleProofParser
  -- Getting the parsed recipient from the combined proof and deserialising to
  -- an address
  let
    recipient = (unwrap (unwrap cmp).transaction).recipient
    maybeAddr = addressFromBech32Bytes recipient
  case maybeAddr of
    Just addr → pure (cmp /\ addr)
    Nothing → readerError "Couldn't convert recipient bech32 to Plutus address"

-- | Parse ByteArray from hexadecimal representation
byteArray ∷ ReadM ByteArray
byteArray = maybeReader hexToByteArray

-- | Parses a EcdsaSecp256k1PubKey from hexadecimal representation.
-- | See `EcdsaSecp256k1PubKey` for the invariants.
ecdsaSecp256k1PublicKey ∷ ReadM EcdsaSecp256k1PubKey
ecdsaSecp256k1PublicKey = maybeReader
  $ Utils.Crypto.ecdsaSecp256k1PubKey
  <=< hexToByteArray

-- | Parses a EcdsaSecp256k1PrivateKey from hexadecimal representation.
ecdsaSecp256k1PrivateKey ∷ ReadM EcdsaSecp256k1PrivateKey
ecdsaSecp256k1PrivateKey = maybeReader
  $ Utils.Crypto.ecdsaSecp256k1PrivateKey
  <=< hexToByteArray

-- | Parses a schnorr private key from the hexadecimal representation
schnorrSecp256k1PrivateKey ∷ ReadM SchnorrSecp256k1PrivateKey
schnorrSecp256k1PrivateKey = maybeReader
  $ Utils.SchnorrSecp256k1.parsePrivateKey
  <=< hexToByteArray

-- | Parses a SidechainSignature from hexadecimal representation.
-- | See `SidechainSignature` for the invariants.
sidechainSignature ∷ ReadM EcdsaSecp256k1Signature
sidechainSignature = maybeReader
  $ Utils.Crypto.ecdsaSecp256k1Signature
  <=< hexToByteArray

-- | Parses a PubKeyHash from hexadecimal representation.
pubKeyHash ∷ ReadM PubKeyHash
pubKeyHash = maybeReader
  $ hexToByteArray
  >=> ed25519KeyHashFromBytes
  >=> PubKeyHash
  >>> pure

-- | Parses a GovernanceAuthority from hexadecimal representation.
governanceAuthority ∷ ReadM Governance.GovernanceAuthority
governanceAuthority =
  Governance.mkGovernanceAuthority <$> pubKeyHash

-- | Parse only CBOR encoded hexadecimal
-- Note: This assumes there will be some validation with the CborBytes, otherwise
-- we should simplify the code and fall back to ByteArray.
cbor ∷ ReadM CborBytes
cbor = cborBytesFromByteArray <$> byteArray

-- | Parse BigInt
bigInt ∷ ReadM BigInt
bigInt = maybeReader BigInt.fromString

-- | Parse a permissioned candidates count
permissionedCandidatesCount ∷ ReadM BigInt
permissionedCandidatesCount = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i >= zero → pure i
        | otherwise → Left "permissioned-candidates-count must be non-negative"
      Nothing → Left "failed to parse int permissioned-candidates-count"
  )

-- | Parse a registered candidates count
registeredCandidatesCount ∷ ReadM BigInt
registeredCandidatesCount = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i >= zero → pure i
        | otherwise → Left "registered-candidates-count must be non-negative"
      Nothing → Left "failed to parse int registered-candidates-count"
  )

-- | Parse a numerator in the threshold.
numerator ∷ ReadM BigInt
numerator = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i >= zero → pure i
        | otherwise → Left "numerator must be non-negative"
      Nothing → Left "failed to parse int numerator"
  )

-- | Parse a denominator in the threshold.
denominator ∷ ReadM BigInt
denominator = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i > zero → pure i
        | otherwise → Left "denominator must be positive"
      Nothing → Left "failed to parse int denominator"
  )

-- | Parse an epoch.
epoch ∷ ReadM BigInt
epoch = eitherReader
  ( \str → case BigInt.fromString str of
      Just i
        | i >= zero → pure i
        | otherwise → Left "epoch must be non negative"
      Nothing → Left "failed to parse int epoch"
  )

-- | Parse UInt
uint ∷ ReadM UInt
uint = maybeReader UInt.fromString

-- | Parses the raw bytes of a `RootHash`
rootHash ∷ ReadM RootHash
rootHash = maybeReader (MerkleTree.rootHashFromByteArray <=< hexToByteArray)

sidechainAddress ∷ ReadM ByteArray
sidechainAddress = hexString

blockHash ∷ ReadM ByteArray
blockHash = hexString

-- | ```
-- | hexString
-- |        -> 0x hexStr
-- |        -> hexStr
-- | ```
-- where `hexStr` is a sequence of hex digits.
hexString ∷ ReadM ByteArray
hexString = maybeReader $ \str →
  case split (Pattern "0x") str of
    [ "", hex ] → hexToByteArray hex
    [ hex ] → hexToByteArray hex
    _ → Nothing

-- | `committeeSignature` is a the CLI parser for `parsePubKeyAndSignature`.
committeeSignature ∷
  ReadM (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)
committeeSignature = maybeReader parsePubKeyAndSignature

-- | `parsePubKeyAndSignature` parses the following format `hexStr[:[hexStr]]`
-- | subject to the hex strings satisfying some conditions about whether the
-- | public key  / signature could be a `EcdsaSecp256k1PubKey` or a
-- | `SidechainSignature`
-- Note: should we make this more strict and disallow `aa:`? in a sense:
-- `aa` denotes a pubkey without a signature
-- `aa:bb` denotes a pubkey and a signature
-- anything else is likely an error, and should be treated as malformed input
parsePubKeyAndSignature ∷
  String → Maybe (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)
parsePubKeyAndSignature str =
  case split (Pattern ":") str of
    [ l, r ] | l /= "" → do
      l' ← Utils.Crypto.ecdsaSecp256k1PubKey <=< hexToByteArray $ l
      if r == "" then pure $ l' /\ Nothing
      else do
        r' ← Utils.Crypto.ecdsaSecp256k1Signature <=< hexToByteArray $ r
        pure $ l' /\ Just r'
    [ l ] → ado
      l' ← Utils.Crypto.ecdsaSecp256k1PubKey <=< hexToByteArray $ l
      in l' /\ Nothing
    _ → Nothing

-- | `pubKeyBytesAndSignatureBytes` is a the CLI parser for `parsePubKeyBytesAndSignatureBytes`.
pubKeyBytesAndSignatureBytes ∷ ReadM (ByteArray /\ Maybe ByteArray)
pubKeyBytesAndSignatureBytes = maybeReader parsePubKeyBytesAndSignatureBytes

-- | `parsePubKeyBytesAndSignatureBytes` parses the following format
-- | `hexStr[:[hexStr]]`
-- Note: should we make this more strict and disallow `aa:`? in a sense:
-- `aa` denotes a pubkey without a signature
-- `aa:bb` denotes a pubkey and a signature
-- anything else is likely an error, and should be treated as malformed input
parsePubKeyBytesAndSignatureBytes ∷
  String → Maybe (ByteArray /\ Maybe ByteArray)
parsePubKeyBytesAndSignatureBytes str =
  case split (Pattern ":") str of
    [ l, r ] | l /= "" → do
      l' ← hexToByteArray $ l
      if r == "" then pure $ l' /\ Nothing
      else do
        r' ← hexToByteArray $ r
        pure $ l' /\ Just r'
    [ l ] → ado
      l' ← hexToByteArray $ l
      in l' /\ Nothing
    _ → Nothing

permissionedCandidateKeys ∷
  ReadM
    { mainchainKey ∷ ByteArray
    , sidechainKey ∷ ByteArray
    , authorityDiscoveryKey ∷ ByteArray
    , grandpaKey ∷ ByteArray
    }
permissionedCandidateKeys = eitherReader parsePermissionedCandidateKeys

parsePermissionedCandidateKeys ∷
  String →
  Either String
    { mainchainKey ∷ ByteArray
    , sidechainKey ∷ ByteArray
    , authorityDiscoveryKey ∷ ByteArray
    , grandpaKey ∷ ByteArray
    }
parsePermissionedCandidateKeys str =
  case split (Pattern ":") str of
    [ mainchainKey', sidechainKey', authorityDiscoveryKey', grandpaKey' ] → do
      mainchainKey ← Either.note ("mainchainKey must be a valid hex string") $
        hexToByteArray mainchainKey'
      sidechainKey ← Either.note ("sidechainKey must be a valid hex string") $
        hexToByteArray sidechainKey'
      authorityDiscoveryKey ←
        Either.note ("authorityDiscoveryKey must be a valid hex string") $
          hexToByteArray authorityDiscoveryKey'
      grandpaKey ← Either.note ("grandpaKey must be a valid hex string") $
        hexToByteArray grandpaKey'
      pure $
        { mainchainKey
        , sidechainKey
        , authorityDiscoveryKey
        , grandpaKey
        }
    _ → Left
      "permissioned-candidate-keys must be a 4 hex strings concatenated with colons, for example: aa:bb:cc:dd"

-- | `parseTokenName` is a thin wrapper around `Contract.Value.mkTokenName` for
-- | converting hex encoded strings to token names
parseTokenName ∷ String → Maybe (TokenName)
parseTokenName hexStr = do
  ba ← hexToByteArray hexStr
  Value.mkTokenName ba

-- | `tokenName` wraps `parseTokenName`.
tokenName ∷ ReadM TokenName
tokenName = maybeReader parseTokenName
