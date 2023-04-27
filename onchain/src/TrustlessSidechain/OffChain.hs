{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- | 'TrustlessSidechain.OffChain' provides utilities for doing offchain
 functionality. In particular, we provide utilities for generating signatures
 / related messages for the system. This is useful for testing the system
 externally.

 See: the executable `trustless-sidechain-gen-signatures`
-}
module TrustlessSidechain.OffChain (
  Bech32Recipient (bech32RecipientBytes),
  signWithSPOKey,
  signWithSidechainKey,
  showTxOutRef,
  showBS,
  showBuiltinBS,
  showRootHash,
  showPubKey,
  showScPubKey,
  showScPubKeyAndSig,
  showSig,
  showThreshold,
  showMerkleTree,
  showMerkleProof,
  showSecpPrivKey,
  showGenesisHash,
  showCombinedMerkleProof,
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
  txOutRefFromText,
) where

-- we import Prelude unqualified here because this module is
-- used for Prelude projects that just generate data for the sidechain

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
import Data.Aeson.Extras qualified as Aeson.Extras
import Data.Aeson.Types qualified as Aeson.Types
import Data.Attoparsec.Text qualified as Attoparsec.Text
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Hash (blake2b_256)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (PubKey (PubKey), Signature (Signature))
import Ledger.Crypto qualified as Crypto
import Plutus.V2.Ledger.Api (
  BuiltinByteString,
  LedgerBytes (LedgerBytes),
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as Builtins.Internal
import TrustlessSidechain.MerkleTree (MerkleProof, MerkleTree, RootHash)
import TrustlessSidechain.Types (
  BlockProducerRegistrationMsg,
  CombinedMerkleProof,
  GenesisHash (getGenesisHash),
  SidechainPubKey (SidechainPubKey),
 )
import Prelude

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

{- | 'Bech32' is a thin wrapper around decoding a bech32 encoded value. See the
 bitcoin reference [3] for more details on the encoding.
-}
data Bech32 = Bech32
  { bech32HumanReadablePart :: HumanReadablePart
  , bech32DataPart :: DataPart
  }
  deriving (Show, Eq)

-- | Converts the data part into bytes
bech32DataPartBytes :: Bech32 -> Maybe ByteString
bech32DataPartBytes = Bech32.dataPartToBytes . bech32DataPart

{- | 'Bech32Recipient' is a newtype wrapper around the binary data part of
 specialized Bech32 type meant to only represent recipients on the main chain...

 In particular, we verify that the human readable part is @addr@ or @addr_test@.
 See Discussion on prefixes in Cardano [2]

 This exists so we can parse
 JSON of 'TrustlessSidechain.Types.MerkleTreeEntry'...
-}
newtype Bech32Recipient = Bech32Recipient {bech32RecipientBytes :: BuiltinByteString}
  deriving (Show, Eq)

-- | 'bech32RecipientFromText' parses a Bech32Recipient from 'Text'.
bech32RecipientFromText :: Text -> Either String Bech32Recipient
bech32RecipientFromText str =
  case Bech32.decode str of
    Left err -> Left $ "Failed decoding bech32: " ++ show err
    Right (bech32HumanReadablePart, bech32DataPart)
      | isAddr -> case bech32DataPartBytes Bech32 {..} of
        Just bs -> Right $ Bech32Recipient $ Builtins.Internal.BuiltinByteString bs
        Nothing -> Left "Failed decoding bytes in bech32 recipient"
      | otherwise ->
        Left $
          List.unwords
            [ "Expected human readable part to be either:"
            , surroundAndShowTextWithBackticks $ Bech32.humanReadablePartToText Bech32.Prefixes.addr
            , "or"
            , surroundAndShowTextWithBackticks $ Bech32.humanReadablePartToText Bech32.Prefixes.addr_test
            ]
      where
        surroundAndShowTextWithBackticks :: Text -> String
        surroundAndShowTextWithBackticks t = "`" ++ show t ++ "`"
        isAddr :: Bool
        isAddr =
          bech32HumanReadablePart == Bech32.Prefixes.addr
            || bech32HumanReadablePart == Bech32.Prefixes.addr_test

instance FromJSON Bech32Recipient where
  parseJSON = Aeson.withText "bech32" $ \str -> case bech32RecipientFromText str of
    Left err -> Aeson.Types.parseFail err
    Right bech32 -> pure bech32

-- * Convenient sidechain committee public / private key wrapper + utility

-- parsing functions

-- | SidechainCommitteeMember is a sidechain (SECP) public and private key pair
data SidechainCommitteeMember = SidechainCommitteeMember
  { scmPrivateKey :: SECP.SecKey
  , scmPublicKey :: SidechainPubKey
  }

{- | 'SidechainCommittee' is a newtype wrapper around a lsit of
 @[SidechainCommitteeMember]@ to provide JSON parsing of a list of committee
 members (i.e., the 'Data.Aeson.FromJSON' is a derived via the newtype
 wrapper)
-}
newtype SidechainCommittee = SidechainCommittee
  {unSidechainCommittee :: [SidechainCommitteeMember]}
  deriving newtype (FromJSON, ToJSON)

instance FromJSON SidechainCommitteeMember where
  parseJSON = Aeson.withObject "SidechainCommitteeMember" $ \v ->
    -- wraps up 'strToSecpPrivKey' and 'strToSecpPubKey' as JSON
    -- parsers.
    let pPrivKey :: Aeson.Value -> Aeson.Types.Parser SECP.SecKey
        pPrivKey (Aeson.String text) =
          case strToSecpPrivKey (Text.unpack text) of
            Left err -> Aeson.Types.parseFail err
            Right ans -> pure ans
        pPrivKey _ = Aeson.Types.parseFail "Expected hex encoded SECP private key"

        pPubKey :: Aeson.Value -> Aeson.Types.Parser SidechainPubKey
        pPubKey (Aeson.String text) =
          case fmap secpPubKeyToSidechainPubKey $ strToSecpPubKey (Text.unpack text) of
            Left err -> Aeson.Types.parseFail err
            Right ans -> pure ans
        pPubKey _ = Aeson.Types.parseFail "Expected hex encoded DER SECP public key"
     in SidechainCommitteeMember
          <$> Aeson.Types.explicitParseField pPrivKey v "private-key"
          <*> Aeson.Types.explicitParseField pPubKey v "public-key"

instance ToJSON SidechainCommitteeMember where
  toJSON (SidechainCommitteeMember {..}) =
    Aeson.object
      [ "private-key" Aeson..= showSecpPrivKey scmPrivateKey
      , "public-key" Aeson..= showScPubKey scmPublicKey
      ]

-- | Parses a hex encoded string into a sidechain private key
strToSecpPrivKey :: String -> Either String SECP.SecKey
strToSecpPrivKey raw = do
  decoded <-
    Bifunctor.first ("Invalid sidechain key hex: " <>)
      . Base16.decode
      . Char8.pack
      $ raw
  maybe (Left "Unable to parse sidechain private key") Right $ SECP.secKey decoded

{- | Parses a hex encoded string into a sidechain public key. Note that this
 uses 'Crypto.Secp256k1.importPubKey' which imports a DER-encoded
 public key.
-}
strToSecpPubKey :: String -> Either String SECP.PubKey
strToSecpPubKey raw = do
  decoded <-
    Bifunctor.first ("Invalid sidechain public key hex: " <>)
      . Base16.decode
      . Char8.pack
      $ raw
  maybe (Left "Unable to parse sidechain public key") Right $ SECP.importPubKey decoded

-- * Generating a private sidechain key

{- | Generates a random sidechain private key.

 The implementation is a translation of the underlying example in the C
 library: https://github.com/bitcoin-core/secp256k1/blob/master/examples/ecdsa.c
 where we use the module 'Crypto.Random' from cryptonite to handle the cross
 platform random bytestring generation

 Surprisingly, the Haskell library we're using doesn't include a nice wrapper
 for this, so we do it ourselves I guess.

 Some implementation notes:

      - We use the default context 'Crypto.Secp256k1.Internal.ctx' which is
      thread safe and used internally for precomputed values (and adding
      randomization to prevent side channel attacks)

      - As given in the C example, we loop forever until we find a "valid"
      key i.e., a non-zero key smaller than the group's order (see
      [here](https://github.com/bitcoin-core/secp256k1/blob/44c2452fd387f7ca604ab42d73746e7d3a44d8a2/include/secp256k1.h#L608)...
      Ostensibly, the probability of a secret key being "invalid" is
      neglible, so we _expect_ this loop to run at most once.

 TODO: might be a good idea to put this in a newtype wrapper...
-}
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
          Just bs' | ret == 1 -> return bs'
          _ -> go
   in go

-- * Signing a message

-- | Sign a message with an Ed25519DSIGN key
signWithSPOKey ::
  SignKeyDSIGN Ed25519DSIGN ->
  BlockProducerRegistrationMsg ->
  Crypto.Signature
signWithSPOKey skey msg =
  let serialised = Builtins.fromBuiltin $ Builtins.serialiseData $ toBuiltinData msg
   in Crypto.Signature
        . Builtins.toBuiltin
        . rawSerialiseSigDSIGN
        $ signDSIGN () serialised skey

{- | Given a @message@, converts the @message@ to the 'BuiltinData'
 representation, seralises to cbor, then takes the @blake2b_256@ hash.
 Finally, this signs the hash with the given SECP256K1 key
-}
signWithSidechainKey ::
  ToData a =>
  SECP.SecKey ->
  a ->
  Crypto.Signature
signWithSidechainKey skey msg =
  let serialised = Builtins.serialiseData $ toBuiltinData msg
      hashedMsg = blake2b_256 $ Builtins.fromBuiltin serialised
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg
   in Crypto.Signature
        . Builtins.toBuiltin
        . SECP.getCompactSig
        . SECP.exportCompactSig
        $ SECP.signMsg skey ecdsaMsg

-- * Parsing functions

{- | 'txOutRefFromText' parses a 'TxOutRef' from 'Text'. E.g., it'll parse
 something like:
 @
  c97c374fa579742fb7934b9a9c306734fdc0d48432d4d6b46498c8288b88100c#0
 @
-}
txOutRefFromText :: Text -> Either String TxOutRef
txOutRefFromText = Attoparsec.Text.parseOnly $ do
  rawTxId <- Attoparsec.Text.takeWhile (/= '#')
  txId <- case Aeson.Extras.tryDecode rawTxId of
    Left err -> fail err
    Right res -> return $ TxId $ Builtins.Internal.BuiltinByteString res

  _ <- Attoparsec.Text.char '#'
  txIx <- Attoparsec.Text.decimal
  pure $ TxOutRef txId txIx

-- * Show functions

-- | Serialise transaction output reference into CLI format (TX_ID#TX_IDX)
showTxOutRef :: TxOutRef -> String
showTxOutRef (TxOutRef (TxId txId) txIdx) =
  showBuiltinBS txId ++ "#" ++ show txIdx

-- | Serialise a ByteString into hex string
showBS :: ByteString -> String
showBS =
  Char8.unpack . Base16.encode

-- | Serialise a ByteString into hex string
showGenesisHash :: GenesisHash -> String
showGenesisHash = showBuiltinBS . getGenesisHash

-- | Serialise a BuiltinByteString into hex string
showBuiltinBS :: BuiltinByteString -> String
showBuiltinBS = showBS . Builtins.fromBuiltin

-- | Serialise a RootHash into hex of serialized built in data.
showRootHash :: RootHash -> String
showRootHash = showHexOfCborBuiltinData

-- | Serialise public key
showPubKey :: PubKey -> String
showPubKey (PubKey (LedgerBytes pk)) = showBuiltinBS pk

-- | Serialise sidechain public key
showScPubKey :: SidechainPubKey -> String
showScPubKey (SidechainPubKey pk) = showBuiltinBS pk

-- | Serailises a 'SECP.SecKey' private key by hex encoding it
showSecpPrivKey :: SECP.SecKey -> String
showSecpPrivKey = showBS . SECP.getSecKey

{- | Serialise a sidechain public key and signature into
 > PUBKEY:SIG
-}
showScPubKeyAndSig :: SidechainPubKey -> Signature -> String
showScPubKeyAndSig sckey sig = concat [showScPubKey sckey, ":", showSig sig]

-- | Serialise signature
showSig :: Signature -> String
showSig (Signature sig) = showBuiltinBS sig

{- | 'showThreshold' shows integers @n@ and @m@ as
 > n/m
 Importantly, this is compatible with the purescript parser format
-}
showThreshold :: Integer -> Integer -> String
showThreshold n m = show n ++ "/" ++ show m

{- | 'showMerkleTree' seralises a merkle tree to the hex encoded cbor builtin
 data representation
-}
showMerkleTree :: MerkleTree -> String
showMerkleTree = showHexOfCborBuiltinData

{- | 'showMerkleProof' seralises a merkle tree proof to the hex encoded cbor builtin
 data representation
-}
showMerkleProof :: MerkleProof -> String
showMerkleProof = showHexOfCborBuiltinData

{- | 'showCombinedMerkleProof' seralises a combined merkle proof to the hex encoded
 cbor builtin data representation
-}
showCombinedMerkleProof :: CombinedMerkleProof -> String
showCombinedMerkleProof = showHexOfCborBuiltinData

{- | 'showHexOfCborBuiltinData' shows the hex of the cbor serialized
 BuiltinData representation of the given argument.

 Many serialization mechanisms are an alias of this.
-}
showHexOfCborBuiltinData :: ToData a => a -> String
showHexOfCborBuiltinData = showBuiltinBS . Builtins.serialiseData . toBuiltinData

-- * Covnerting converting private keys to public keys

-- | Derive Ed25519DSIGN public key from the private key
toSpoPubKey :: SignKeyDSIGN Ed25519DSIGN -> Crypto.PubKey
toSpoPubKey =
  vKeyToSpoPubKey
    . deriveVerKeyDSIGN

-- | Converts Ed25519DSIGN public key to a PubKey.
vKeyToSpoPubKey :: VerKeyDSIGN Ed25519DSIGN -> Crypto.PubKey
vKeyToSpoPubKey =
  Crypto.PubKey
    . LedgerBytes
    . Builtins.toBuiltin
    . rawSerialiseVerKeyDSIGN @Ed25519DSIGN

-- | Derive SECP256K1 public key from the private key
toSidechainPubKey :: SECP.SecKey -> SidechainPubKey
toSidechainPubKey =
  secpPubKeyToSidechainPubKey
    . SECP.derivePubKey

-- | Converts a 'SECP.PubKey' to a 'SidechainPubKey'
secpPubKeyToSidechainPubKey :: SECP.PubKey -> SidechainPubKey
secpPubKeyToSidechainPubKey = SidechainPubKey . Builtins.toBuiltin . SECP.exportPubKey True
