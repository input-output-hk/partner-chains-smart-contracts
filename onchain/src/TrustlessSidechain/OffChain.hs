{-# LANGUAGE RecordWildCards #-}

-- | 'TrustlessSidechain.OffChain' provides utilities for doing offchain
-- functionality. In particular, we provide utilities for generating signatures
-- / related messages for the system. This is useful for testing the system
-- externally.
--
-- See: the executable `trustless-sidechain-gen-signatures`
module TrustlessSidechain.OffChain (
  Bech32Recipient (bech32RecipientBytes),
  signWithSPOKey,
  signWithSidechainKey,
  encodeTxOutRef,
  encodeHexBuiltinBS,
  encodeScPubKeyAndSig,
  showThreshold,
  encodeHexMerkleTree,
  encodeHexMerkleProof,
  encodeHexSecpPrivKey,
  encodeHexCombinedMerkleProof,
  encodeHexOfCborBuiltinData,
  toSpoPubKey,
  vKeyToSpoPubKey,
  toSidechainPubKey,
  secpPubKeyToSidechainPubKey,
  generateRandomSecpPrivKey,
  SidechainCommittee (..),
  SidechainCommitteeMember (..),
  strToSecpPrivKey,
  strToSecpPubKey,
  bech32RecipientFromText,
  ATMSKind (..),
  showATMSKind,
  parseATMSKind,
) where

import Cardano.Codec.Bech32.Prefixes qualified as Bech32.Prefixes
import Cardano.Crypto.DSIGN (Ed25519DSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Codec.Binary.Bech32 (DataPart, HumanReadablePart)
import Codec.Binary.Bech32 qualified as Bech32
import Crypto.Random qualified as Random
import Crypto.Secp256k1 qualified as SECP
import Crypto.Secp256k1.Internal qualified as SECP.Internal
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Hash (blake2b_256)
import Data.String qualified as HString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Err (undefined)
import Plutus.V1.Ledger.Bytes qualified as Plutus
import Plutus.V2.Ledger.Api (
  BuiltinByteString,
  LedgerBytes (LedgerBytes),
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as Builtins.Internal
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.MerkleTree (MerkleProof, MerkleTree)
import TrustlessSidechain.Types (
  BlockProducerRegistrationMsg,
  CombinedMerkleProof,
  EcdsaSecp256k1PubKey (EcdsaSecp256k1PubKey, getEcdsaSecp256k1PubKey),
  PubKey (PubKey),
  Signature (Signature, getSignature),
 )

-- * Bech32 addresses

-- For references, see:
--
--      [1] The bitcoin specification:
--      https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
--
--      [2] Discussion on prefixes in Cardano:
--      https://github.com/cardano-foundation/CIPs/tree/master/CIP-0005
--
--      [3] The CIP for the data part (fairly certain that's what it's talking
--      about although it doesn't explicitly say that):
--      https://cips.cardano.org/cips/cip19/

-- | 'Bech32' is a thin wrapper around decoding a bech32 encoded value. See the
-- bitcoin reference [3] for more details on the encoding.
data Bech32 = Bech32
  { bech32HumanReadablePart :: HumanReadablePart
  , bech32DataPart :: DataPart
  }
  deriving (Show, Eq)

-- | Converts the data part into bytes
bech32DataPartBytes :: Bech32 -> Maybe ByteString
bech32DataPartBytes = Bech32.dataPartToBytes . bech32DataPart

-- | 'Bech32Recipient' is a newtype wrapper around the binary data part of
-- specialized Bech32 type meant to only represent recipients on the main chain...
--
-- In particular, we verify that the human readable part is @addr@ or @addr_test@.
-- See Discussion on prefixes in Cardano [2]
--
-- This exists so we can parse
-- JSON of 'TrustlessSidechain.Types.MerkleTreeEntry'...
newtype Bech32Recipient = Bech32Recipient {bech32RecipientBytes :: BuiltinByteString}
  deriving (Show, Eq)

-- | 'bech32RecipientFromText' parses a Bech32Recipient from 'Text'.
bech32RecipientFromText :: Text -> Either Text Bech32Recipient
bech32RecipientFromText str =
  case Bech32.decode str of
    Left err -> Left $ "Failed decoding bech32: " <> Text.pack (show err)
    Right (bech32HumanReadablePart, bech32DataPart)
      | isAddr -> case bech32DataPartBytes Bech32 {..} of
        Just bs -> Right $ Bech32Recipient $ Builtins.Internal.BuiltinByteString bs
        Nothing -> Left "Failed decoding bytes in bech32 recipient"
      | otherwise ->
        Left $
          Text.unwords
            [ "Expected human readable part to be either:"
            , surroundAndShowTextWithBackticks $ Bech32.humanReadablePartToText Bech32.Prefixes.addr
            , "or"
            , surroundAndShowTextWithBackticks $ Bech32.humanReadablePartToText Bech32.Prefixes.addr_test
            ]
      where
        surroundAndShowTextWithBackticks :: Text -> Text
        surroundAndShowTextWithBackticks t = "`" <> t <> "`"
        isAddr :: Bool
        isAddr =
          bech32HumanReadablePart == Bech32.Prefixes.addr
            || bech32HumanReadablePart == Bech32.Prefixes.addr_test

instance FromJSON Bech32Recipient where
  parseJSON = Aeson.withText "bech32" $ \str -> case bech32RecipientFromText str of
    Left err -> Aeson.Types.parseFail (Text.unpack err)
    Right bech32 -> pure bech32

-- * Convenient sidechain committee public / private key wrapper + utility

-- parsing functions

-- | SidechainCommitteeMember is a sidechain (SECP) public and private key pair
data SidechainCommitteeMember = SidechainCommitteeMember
  { scmPrivateKey :: SECP.SecKey
  , -- | @since v4.0.0
    scmPublicKey :: EcdsaSecp256k1PubKey
  }

-- | 'SidechainCommittee' is a newtype wrapper around a lsit of
-- @[SidechainCommitteeMember]@ to provide JSON parsing of a list of committee
-- members (i.e., the 'Data.Aeson.FromJSON' is a derived via the newtype
-- wrapper)
newtype SidechainCommittee = SidechainCommittee
  {unSidechainCommittee :: [SidechainCommitteeMember]}
  deriving newtype (FromJSON, ToJSON)

instance FromJSON SidechainCommitteeMember where
  parseJSON = Aeson.withObject "SidechainCommitteeMember" $ \v ->
    -- wraps up 'strToSecpPrivKey' and 'strToSecpPubKey' as JSON
    -- parsers.
    let pPrivKey :: Aeson.Value -> Aeson.Types.Parser SECP.SecKey
        pPrivKey (Aeson.String text) =
          case strToSecpPrivKey $ Text.encodeUtf8 text of
            Left err -> Aeson.Types.parseFail err
            Right ans -> pure ans
        pPrivKey _ = Aeson.Types.parseFail "Expected hex encoded SECP private key"

        pPubKey :: Aeson.Value -> Aeson.Types.Parser EcdsaSecp256k1PubKey
        pPubKey (Aeson.String text) =
          case fmap secpPubKeyToSidechainPubKey $ strToSecpPubKey $ Text.encodeUtf8 text of
            Left err -> Aeson.Types.parseFail err
            Right ans -> pure ans
        pPubKey _ = Aeson.Types.parseFail "Expected hex encoded DER SECP public key"
     in SidechainCommitteeMember
          <$> Aeson.Types.explicitParseField pPrivKey v "private-key"
          <*> Aeson.Types.explicitParseField pPubKey v "public-key"

-- | @since v3.0.0
instance ToJSON SidechainCommitteeMember where
  toJSON (SidechainCommitteeMember {..}) =
    Aeson.object
      [ "private-key" Aeson..= Text.decodeUtf8 (encodeHexSecpPrivKey scmPrivateKey)
      , "public-key" Aeson..= show scmPublicKey
      ]
  toEncoding (SidechainCommitteeMember {..}) =
    Aeson.pairs
      ( "private-key"
          Aeson..= Text.decodeUtf8 (encodeHexSecpPrivKey scmPrivateKey)
          <> "public-key"
          Aeson..= show scmPublicKey
      )

-- | Parses a hex encoded string into a sidechain private key
strToSecpPrivKey ::
  ByteString ->
  Either HString.String SECP.SecKey
strToSecpPrivKey raw = do
  decoded <-
    Bifunctor.first ("Invalid sidechain key hex: " <>)
      . Base16.decode
      $ raw
  maybe (Left "Unable to parse sidechain private key") Right $ SECP.secKey decoded

-- | Parses a hex encoded string into a sidechain public key. Note that this
-- uses 'Crypto.Secp256k1.importPubKey' which imports a DER-encoded
-- public key.
strToSecpPubKey ::
  ByteString ->
  Either HString.String SECP.PubKey
strToSecpPubKey raw = do
  decoded <-
    Bifunctor.first ("Invalid sidechain public key hex: " <>)
      . Base16.decode
      $ raw
  maybe (Left "Unable to parse sidechain public key") Right $ SECP.importPubKey decoded

-- * Generating a private sidechain key

-- | Generates a random sidechain private key.
--
-- The implementation is a translation of the underlying example in the C
-- library: https://github.com/bitcoin-core/secp256k1/blob/master/examples/ecdsa.c
-- where we use the module 'Crypto.Random' from cryptonite to handle the cross
-- platform random bytestring generation
--
-- Surprisingly, the Haskell library we're using doesn't include a nice wrapper
-- for this, so we do it ourselves I guess.
--
-- Some implementation notes:
--
--      - We use the default context 'Crypto.Secp256k1.Internal.ctx' which is
--      thread safe and used internally for precomputed values (and adding
--      randomization to prevent side channel attacks)
--
--      - As given in the C example, we loop forever until we find a "valid"
--      key i.e., a non-zero key smaller than the group's order (see
--      [here](https://github.com/bitcoin-core/secp256k1/blob/44c2452fd387f7ca604ab42d73746e7d3a44d8a2/include/secp256k1.h#L608)...
--      Ostensibly, the probability of a secret key being "invalid" is
--      neglible, so we _expect_ this loop to run at most once.
--
-- TODO: might be a good idea to put this in a newtype wrapper...
generateRandomSecpPrivKey :: IO SECP.SecKey
generateRandomSecpPrivKey =
  let go = do
        bs <- Random.getRandomBytes 32
        ret <- SECP.Internal.useByteString bs $ \(ptr, _len) ->
          SECP.Internal.ecSecKeyVerify SECP.Internal.ctx ptr
        -- Returns 1 in the case that this is valid, see the FFI
        -- call
        -- [here](https://github.com/bitcoin-core/secp256k1/blob/44c2452fd387f7ca604ab42d73746e7d3a44d8a2/include/secp256k1.h#L608)
        case SECP.secKey bs of
          Just bs' | ret == 1 -> pure bs'
          _ -> go
   in go

-- * Signing a message

-- | Sign a message with an Ed25519DSIGN key
signWithSPOKey ::
  SignKeyDSIGN Ed25519DSIGN ->
  BlockProducerRegistrationMsg ->
  Signature
signWithSPOKey skey msg =
  let serialised = Builtins.fromBuiltin $ Builtins.serialiseData $ toBuiltinData msg
   in Signature
        . LedgerBytes
        . Builtins.toBuiltin
        . rawSerialiseSigDSIGN
        $ signDSIGN () serialised skey

-- | Given a @message@, converts the @message@ to the 'BuiltinData'
-- representation, seralises to cbor, then takes the @blake2b_256@ hash.
-- Finally, this signs the hash with the given SECP256K1 key
signWithSidechainKey ::
  ToData a =>
  SECP.SecKey ->
  a ->
  Signature
signWithSidechainKey skey msg =
  let serialised = Builtins.serialiseData $ toBuiltinData msg
      hashedMsg = blake2b_256 $ Builtins.fromBuiltin serialised
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg
   in Signature
        . LedgerBytes
        . Builtins.toBuiltin
        . SECP.getCompactSig
        . SECP.exportCompactSig
        $ SECP.signMsg skey ecdsaMsg

-- * Show functions

-- | Serialise transaction output reference into CLI format (TX_ID#TX_IDX)
encodeTxOutRef ::
  TxOutRef -> ByteString
encodeTxOutRef (TxOutRef (TxId txId) txIdx) =
  encodeHexBuiltinBS txId <> "#" <> ByteString.Char8.pack (show txIdx)

-- | Serialise a BuiltinByteString into hex string
encodeHexBuiltinBS :: BuiltinByteString -> ByteString
encodeHexBuiltinBS = Base16.encode . Builtins.fromBuiltin

-- | Serailises a 'SECP.SecKey' private key by hex encoding it
encodeHexSecpPrivKey :: SECP.SecKey -> ByteString
encodeHexSecpPrivKey = Base16.encode . SECP.getSecKey

-- | Serialise a sidechain public key and signature into
-- > PUBKEY:SIG
encodeScPubKeyAndSig ::
  EcdsaSecp256k1PubKey ->
  Signature ->
  ByteString
encodeScPubKeyAndSig sckey sig =
  ByteString.concat
    [ Plutus.bytes $ getEcdsaSecp256k1PubKey sckey
    , ":"
    , Plutus.bytes $ getSignature sig
    ]

-- | 'showThreshold' shows integers @n@ and @m@ as
-- > n/m
-- Importantly, this is compatible with the purescript parser format
showThreshold ::
  Integer ->
  Integer ->
  Text
showThreshold n m = Text.pack (show n) <> "/" <> Text.pack (show m)

-- | 'encodeHexMerkleTree' seralises a merkle tree to the hex encoded cbor builtin
-- data representation
encodeHexMerkleTree :: MerkleTree -> ByteString
encodeHexMerkleTree = encodeHexOfCborBuiltinData

-- | 'encodeHexMerkleProof' seralises a merkle tree proof to the hex encoded cbor builtin
-- data representation
encodeHexMerkleProof :: MerkleProof -> ByteString
encodeHexMerkleProof = encodeHexOfCborBuiltinData

-- | 'encodeHexCombinedMerkleProof' seralises a combined merkle proof to the hex encoded
-- cbor builtin data representation
encodeHexCombinedMerkleProof :: CombinedMerkleProof -> ByteString
encodeHexCombinedMerkleProof = encodeHexOfCborBuiltinData

-- | 'encodeHexOfCborBuiltinData' shows the hex of the cbor serialized
-- BuiltinData representation of the given argument.
--
-- Many serialization mechanisms are an alias of this.
encodeHexOfCborBuiltinData ::
  forall (a :: Type).
  ToData a =>
  a ->
  ByteString
encodeHexOfCborBuiltinData = encodeHexBuiltinBS . Builtins.serialiseData . toBuiltinData

-- * Converting private keys to public keys

-- | Derive Ed25519DSIGN public key from the private key
toSpoPubKey :: SignKeyDSIGN Ed25519DSIGN -> PubKey
toSpoPubKey =
  vKeyToSpoPubKey
    . deriveVerKeyDSIGN

-- | Converts Ed25519DSIGN public key to a PubKey.
vKeyToSpoPubKey :: VerKeyDSIGN Ed25519DSIGN -> PubKey
vKeyToSpoPubKey =
  PubKey
    . LedgerBytes
    . Builtins.toBuiltin
    . rawSerialiseVerKeyDSIGN @Ed25519DSIGN

-- | Derive SECP256K1 public key from the private key
toSidechainPubKey :: SECP.SecKey -> EcdsaSecp256k1PubKey
toSidechainPubKey =
  secpPubKeyToSidechainPubKey
    . SECP.derivePubKey

-- | Converts a 'SECP.PubKey' to a 'SidechainPubKey'
secpPubKeyToSidechainPubKey :: SECP.PubKey -> EcdsaSecp256k1PubKey
secpPubKeyToSidechainPubKey = EcdsaSecp256k1PubKey . LedgerBytes . Builtins.toBuiltin . SECP.exportPubKey True

-- * ATMS offchain types

-- | 'ATMSKind' denotes the ATMS scheme used for the sidechain
data ATMSKind
  = Plain
  | Multisignature
  | PoK
  | Dummy
  deriving (Eq)

-- | 'showATMSKind' shows the 'ATMSKind' in a CTL compatible way.
showATMSKind :: ATMSKind -> ByteString
showATMSKind Plain = "plain"
showATMSKind _ = error "unimplemented ATMSKind"

-- | 'showATMSKind' shows the 'ATMSKind' in a CTL compatible way.
parseATMSKind :: ByteString -> Maybe ATMSKind
parseATMSKind str = case str of
  "plain" -> Just Plain
  _ -> Nothing
