{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Applicative ((<**>))
import Control.Monad (MonadPlus (mzero), void)
import Crypto.Secp256k1 qualified as SECP
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Hash (blake2b_256)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (PubKey (PubKey), Signature (Signature))
import Ledger.Crypto qualified as Crypto
import Options.Applicative (
  auto,
  eitherReader,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
 )
import Options.Applicative qualified as OptParse
import Plutus.V2.Ledger.Api (
  BuiltinByteString,
  LedgerBytes (LedgerBytes),
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.OffChain.Types (
  GenesisHash (GenesisHash),
  PassiveBrdgSidechainParams (PassiveBrdgSidechainParams, chainId, genesisHash, genesisMint, genesisUtxo),
  SidechainPubKey (SidechainPubKey),
  convertSCParams,
 )
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmInputUtxo,
    bprmSidechainParams,
    bprmSidechainPubKey
  ),
 )
import Prelude hiding (takeWhile)

data Args = Args
  { chainId :: Integer
  , genesisHash :: GenesisHash
  , genesisMint :: Maybe TxOutRef
  , genesisUtxo :: TxOutRef
  , spoPrivKey :: SignKeyDSIGN Ed25519DSIGN
  , sidechainPrivKey :: SECP.SecKey
  , registrationUtxo :: TxOutRef
  }

data RegistrationArgs = RegistrationArgs
  { -- | SPO cold verification key hash
    spoPubKey :: PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    sidechainPubKey :: SidechainPubKey
  , -- | Signature of the SPO
    spoSignature :: Signature
  , -- | Signature of the SPO
    sidechainSignature :: Signature
  }
  deriving stock (Prelude.Show)

main :: IO ()
main = do
  args <- execParser opts

  let scParams =
        PassiveBrdgSidechainParams
          { chainId = args.chainId
          , genesisHash = args.genesisHash
          , genesisMint = args.genesisMint
          , genesisUtxo = args.genesisUtxo -- This is not needed for the Passive Bridge, so we're just using the same tx in
          }

      regData =
        RegistrationArgs
          { spoPubKey = toSpoPubKey args.spoPrivKey
          , sidechainPubKey = toSidechainPubKey args.sidechainPrivKey
          , spoSignature = signWithSPOKey args.spoPrivKey msg
          , sidechainSignature = signWithSidechainKey args.sidechainPrivKey msg
          }
      msg =
        BlockProducerRegistrationMsg
          { bprmSidechainParams = convertSCParams scParams
          , bprmSidechainPubKey = toSidechainPubKey args.sidechainPrivKey
          , bprmInputUtxo = args.registrationUtxo
          }

  putStrLn "Please call ctl-main with the following arguments:"
  putStrLn ""

  putStrLn $
    mconcat $
      unwords
        <$> [ ["nix run .#ctl-main -- register \\\n"]
            , ["--signing-key-file $SIGNING_KEY \\\n"]
            , ["--genesis-committee-hash-utxo", showTxOutRef args.genesisUtxo, "\\\n"]
            , maybe [] (\oref -> ["--genesis-mint-utxo", showTxOutRef oref, "\\\n"]) args.genesisMint
            , ["--sidechain-id", show args.chainId, "\\\n"]
            , ["--sidechain-genesis-hash", show args.genesisHash, "\\\n"]
            , ["--spo-public-key", showPubKey regData.spoPubKey, "\\\n"]
            , ["--sidechain-public-key", showScPubKey regData.sidechainPubKey, "\\\n"]
            , ["--spo-signature", showSig regData.spoSignature, "\\\n"]
            , ["--sidechain-signature", showSig regData.sidechainSignature, "\\\n"]
            , ["--registration-utxo", showTxOutRef args.registrationUtxo]
            ]

opts :: OptParse.ParserInfo Args
opts =
  info
    (argParser <**> helper)
    ( fullDesc
        <> progDesc "Generate arguments for committee candidate registration"
    )

argParser :: OptParse.Parser Args
argParser = do
  chainId <-
    option auto $
      mconcat
        [ short 'i'
        , long "sidechain-id"
        , metavar "1"
        , help "Sidechain ID"
        ]

  genesisMint <-
    optional $
      option parseTxOutRef $
        mconcat
          [ short 'm'
          , long "genesis-mint-utxo"
          , metavar "TX_ID#TX_IDX"
          , help "Input UTxO to be spend with the genesis mint"
          ]

  genesisHash <-
    option
      parseGenesisHash
      $ mconcat
        [ short 'h'
        , long "sidechain-genesis-hash"
        , metavar "GENESIS_HASH"
        , help "Sidechain genesis hash"
        ]

  genesisUtxo <-
    option parseTxOutRef $
      mconcat
        [ short 'c'
        , long "genesis-committee-hash-utxo"
        , metavar "TX_ID#TX_IDX"
        , help "Input UTxO to be spent with the first committee hash setup"
        ]

  spoPrivKey <-
    option (eitherReader toSpoPrivKey) $
      mconcat
        [ long "spo-signing-key"
        , metavar "SIGNING_KEY"
        , help "SPO Cold signing key of the block producer candidate"
        ]

  sidechainPrivKey <-
    option (eitherReader toSidechainPrivKey) $
      mconcat
        [ long "sidechain-signing-key"
        , metavar "SIGNING_KEY"
        , help "Signing key of the sidechain block producer candidate"
        ]

  registrationUtxo <-
    option parseTxOutRef $
      mconcat
        [ long "registration-utxo"
        , metavar "TX_ID#TX_IDX"
        , help "Input UTxO to be spend with the commitee candidate registration"
        ]

  pure $
    Args
      { chainId
      , genesisHash
      , genesisMint
      , genesisUtxo
      , spoPrivKey
      , sidechainPrivKey
      , registrationUtxo
      }
  where
    parseTxOutRef =
      eitherReader
        ( parseOnly txOutRefParser
            . Text.pack
        )

    parseGenesisHash =
      eitherReader
        ( fmap (GenesisHash . Builtins.toBuiltin)
            . mapLeft ("Unable to parse genesisHash: " <>)
            . Base16.decode
            . Char8.pack
        )

-- Keys

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

signWithSidechainKey ::
  SECP.SecKey ->
  BlockProducerRegistrationMsg ->
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

toSpoPrivKey :: String -> Either String (SignKeyDSIGN Ed25519DSIGN)
toSpoPrivKey =
  fmap (genKeyDSIGN @Ed25519DSIGN . mkSeedFromBytes)
    . mapLeft ("Invalid spo key hex: " <>)
    . Base16.decode
    . Char8.pack

toSpoPubKey :: SignKeyDSIGN Ed25519DSIGN -> Crypto.PubKey
toSpoPubKey =
  Crypto.PubKey
    . LedgerBytes
    . Builtins.toBuiltin
    . rawSerialiseVerKeyDSIGN @Ed25519DSIGN
    . deriveVerKeyDSIGN

toSidechainPrivKey :: String -> Either String SECP.SecKey
toSidechainPrivKey raw = do
  decoded <-
    mapLeft ("Invalid sidechain key hex: " <>)
      . Base16.decode
      . Char8.pack
      $ raw
  maybeToRight "Unable to parse sidechain private key" $ SECP.secKey decoded

toSidechainPubKey :: SECP.SecKey -> SidechainPubKey
toSidechainPubKey =
  SidechainPubKey
    . Builtins.toBuiltin
    . SECP.exportPubKey True
    . SECP.derivePubKey

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- TxId <$> decodeHash (takeWhile (/= '#'))
  void $ char '#'

  txIx <- decimal
  pure $ TxOutRef txId txIx

showTxOutRef :: TxOutRef -> String
showTxOutRef (TxOutRef (TxId txId) txIdx) =
  showBuiltinBS txId ++ "#" ++ show txIdx

showBS :: ByteString -> String
showBS =
  Char8.unpack . Base16.encode

showBuiltinBS :: BuiltinByteString -> String
showBuiltinBS = showBS . Builtins.fromBuiltin

showPubKey :: PubKey -> String
showPubKey (PubKey (LedgerBytes pk)) = showBuiltinBS pk

showScPubKey :: SidechainPubKey -> String
showScPubKey (SidechainPubKey pk) = showBuiltinBS pk

showSig :: Signature -> String
showSig (Signature sig) = showBuiltinBS sig

-- Helpers

decodeHash :: Parser Text -> Parser Builtins.BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . Builtins.toBuiltin) (tryDecode parsed)
