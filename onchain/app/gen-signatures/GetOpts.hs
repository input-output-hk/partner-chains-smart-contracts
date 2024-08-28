{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | The module 'GetOpts' provides functionality / data types to parse the command line
-- arguments
module GetOpts (
  getOpts,
  Args (..),
  Command (..),
  GenCliCommand (..),
  SidechainKeyCommand (..),
) where

import Cardano.Binary qualified as Binary
import Cardano.Crypto.DSIGN (Ed25519DSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  genKeyDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Exception (ioError)
import Control.Monad (MonadPlus (mzero), guard, join, return)
import Crypto.Secp256k1 qualified as SECP
import Data.Aeson qualified as Aeson
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Char8 qualified as Char8
import Data.Char qualified as Char
import Data.Either.Combinators (mapLeft)
import Data.List qualified as List
import Data.String qualified as HString
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (
  auto,
  command,
  eitherReader,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  subparser,
 )
import Options.Applicative qualified as OptParse
import PlutusLedgerApi.V2 (
  PubKeyHash (PubKeyHash),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins qualified as Builtins
import System.IO (FilePath)
import System.IO.Error (userError)
import TrustlessSidechain.Governance.Admin (
  GovernanceAuthority,
  mkGovernanceAuthority,
 )
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.OffChain (
  SidechainCommittee (SidechainCommittee),
  SidechainCommitteeMember (..),
 )
import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (
  EcdsaSecp256k1PubKey,
  SidechainParams (..),
 )

-- | 'getArgs' grabs the command line options ('Args').
getOpts :: IO Args
getOpts = Control.Monad.join $ execParser opts

-- * Types

-- | Parsed arguments of the CLI.
newtype Args = Args
  { aCommand :: Command
  -- ^ The 'Command' to take for the given arguments (i.e., the result of
  -- the subparser.
  }

-- | 'Command' represents the commands that we may execute from the frontend
data Command
  = GenCliCommand
      { gccSigningKeyFile :: FilePath
      , gccSidechainParams :: SidechainParams
      -- ^ The 'String' which identifies the ATMS kind used by the sidechain.
      -- Note that this 'String' is simply forwarded to Purescript CLI.
      , gccCliCommand :: GenCliCommand
      }
  | SidechainKeyCommand {skCommand :: SidechainKeyCommand}

-- | 'SidechainKeyCommand' is for commands related to working with sidechain
-- keys
data SidechainKeyCommand
  = -- | For generating a fresh (with high probability) sidechain private key
    FreshSidechainPrivateKey
  | -- | For generating a file of a fresh (with high probability) sidechain
    -- committee with both their private and public keys.
    FreshSidechainCommittee
      { fscCommitteeSize :: Int
      -- ^ the committee size i.e., how many private / public key pairs
      -- to generate.
      -- Invariant: must be non-negative
      }
  | -- | Converts a sidechain private key to a public key
    SidechainPrivateKeyToPublicKey
      {spktpkPrivateKey :: SECP.SecKey}

--  | 'GenCliCommand' is for commands which generate CLI commands for the
--  purescript interface.
data GenCliCommand
  = -- | CLI arguments for registering an SPO
    RegistrationCommand
      { rcSpoPrivKey :: SignKeyDSIGN Ed25519DSIGN
      -- ^ SPO private key
      , rcSidechainPrivKey :: SECP.SecKey
      -- ^ Private key in the sidechain's desired format
      , rcRegistrationUtxo :: TxOutRef
      -- ^ Utxo of admit candidate registration
      }
  | -- | CLI arguments for deregistering an SPO
    DeregistrationCommand
      { drSpoPubKey :: VerKeyDSIGN Ed25519DSIGN
      -- ^ SPO public key
      }
  | -- | CLI arguments for saving a new merkle root
    InitSidechainCommand
      { iscInitCommitteePubKeys :: [EcdsaSecp256k1PubKey]
      -- ^ initial committee public keys
      -- | @since v4.0.0
      , iscSidechainEpoch :: Integer
      -- ^ inital sidechain epoch
      }

-- * CLI parser

-- | Parser info for the CLI arguments i.e., wraps up 'argParser' with some
-- extra info.
--
-- Note: the return type is @IO Args@ because some CLI commands have an
-- alternative "read a file as input" (isntead of over multiple command line
-- arguments) and we do the IO to read the files here.
--
-- This simplifies the design for phases which process (see the module
-- 'GenOutput') this information.
opts :: OptParse.ParserInfo (IO Args)
opts =
  info
    argParser
    ( fullDesc
        <> progDesc "Internal tool for generating trustless sidechain CLI commands"
    )

-- | Parser for the CLI arguments. Aggregates all the parsers together into a
-- single parser, and includes a @--help@ parser.
argParser :: OptParse.Parser (IO Args)
argParser =
  do
    ioCmd <-
      subparser
        $ mconcat
          [ initSidechainCommand
          , registerCommand
          , deregisterCommand
          , -- generating sidechain keys
            freshSidechainPrivateKeyCommand
          , sidechainPrivateKeyToPublicKeyCommand
          , freshSidechainCommittee
          ]

    pure $ Args <$> ioCmd
    <**> helper

-- | 'parseTxOutRef' parses the CLI flag value
-- > HEXSTR#UINT
parseTxOutRef :: OptParse.ReadM TxOutRef
parseTxOutRef =
  eitherReader
    ( parseOnly txOutRefParser
        . Text.pack
    )
  where
    -- attoparsec parser for @HEXSTR#UINT@
    txOutRefParser :: Parser TxOutRef
    txOutRefParser = do
      txId <- TxId <$> decodeHash (Data.Attoparsec.Text.takeWhile (/= '#'))
      void $ char '#'
      txIx <- decimal
      pure $ TxOutRef txId txIx

-- | 'parseThreshold' parses the CLI flag value
-- > UINT/UINT
-- where the second UINT must satisfy > 0.
parseThreshold :: OptParse.ReadM (Integer, Integer)
parseThreshold = eitherReader $ parseOnly thresholdParser . Text.pack
  where
    thresholdParser :: Parser (Integer, Integer)
    thresholdParser = do
      numerator <- decimal
      void (char '/')
      denominator <- decimal
      Control.Monad.guard $ denominator > 0
      pure (numerator, denominator)

-- | 'parseSpoPrivKey' parses the CLI flag value which is an SPO private key
parseSpoPrivKey :: OptParse.ReadM (SignKeyDSIGN Ed25519DSIGN)
parseSpoPrivKey = eitherReader toSpoPrivKey
  where
    toSpoPrivKey ::
      HString.String -> Either HString.String (SignKeyDSIGN Ed25519DSIGN)
    toSpoPrivKey =
      fmap (genKeyDSIGN @Ed25519DSIGN . mkSeedFromBytes)
        . mapLeft ("Invalid spo key hex: " <>)
        . Base16.decode
        . Char8.pack

-- | 'parseSpoPrivKey' parses the CLI flag value which is an SPO private key
-- encoded as cbor hex format.
--
-- This is compatible with @cardano-cli@'s output format. In particular, if you
-- generate generate a secret key / private key pair with
-- > cardano-cli address key-gen \
-- >  --verification-key-file payment.vkey \
-- >  --signing-key-file payment.skey
-- Then, the JSON field @cborHex@ of the JSON object in @payment.skey@ is what
-- this will parse.
parseSpoPrivKeyCbor :: OptParse.ReadM (SignKeyDSIGN Ed25519DSIGN)
parseSpoPrivKeyCbor = eitherReader toSpoPrivKeyCbor
  where
    toSpoPrivKeyCbor ::
      HString.String -> Either HString.String (SignKeyDSIGN Ed25519DSIGN)
    toSpoPrivKeyCbor str = do
      bin <-
        mapLeft ("Invalid spo key hex: " <>)
          $ Base16.decode
          . Char8.pack
          $ str
      mapLeft ((<>) "Invalid cbor spo key: " . show) $ Binary.decodeFull' bin

-- | Parse SECP256K1 private key
parseSidechainPrivKey :: OptParse.ReadM SECP.SecKey
parseSidechainPrivKey =
  eitherReader
    $ OffChain.strToSecpPrivKey
    . ByteString.Char8.pack

-- | Parse SECP256K1 public key -- see 'OffChain.strToSecpPubKey' for details
-- on the format
parseSidechainPubKey :: OptParse.ReadM EcdsaSecp256k1PubKey
parseSidechainPubKey =
  eitherReader
    $ fmap OffChain.secpPubKeyToSidechainPubKey
    . OffChain.strToSecpPubKey
    . ByteString.Char8.pack

-- | Parse pub key hash.
parseGovernanceAuthority :: OptParse.ReadM GovernanceAuthority
parseGovernanceAuthority =
  eitherReader
    ( fmap (mkGovernanceAuthority . PubKeyHash . Builtins.toBuiltin)
        . mapLeft ("Invalid public key hash: " <>)
        . Base16.decode
        . Char8.pack
    )

-- | 'parseSpoPubKey' parses the CLI flag value which is an SPO public key
-- encoded as hex cbor format.
--
-- This is compatible with @cardano-cli@'s output format. In particular, if you
-- generate generate a secret key / private key pair with
-- > cardano-cli address key-gen \
-- >  --verification-key-file payment.vkey \
-- >  --signing-key-file payment.skey
-- Then, the JSON field @cborHex@ of the JSON object in @payment.vkey@ is what
-- this will parse.
parseSpoPubKeyCbor :: OptParse.ReadM (VerKeyDSIGN Ed25519DSIGN)
parseSpoPubKeyCbor = eitherReader toSpoPubKeyCbor
  where
    toSpoPubKeyCbor ::
      HString.String -> Either HString.String (VerKeyDSIGN Ed25519DSIGN)
    toSpoPubKeyCbor str = do
      bin <-
        mapLeft ("Invalid spo key hex: " <>)
          $ Base16.decode
          . Char8.pack
          $ str
      mapLeft ((<>) "Invalid cbor spo key: " . show)
        $ Binary.decodeFull' bin

-- Commented out this code for legacy reasons. Originally, we parsed the
-- roothash in its cbor representation, but really we want to actually just
-- parse / output the actual 32 byte root hash.
-- > parseRootHash :: OptParse.ReadM RootHash
-- > parseRootHash = eitherReader $ \str -> do
-- >   binary <- mapLeft ("Invalid root hash of merkle tree hex: " <>) . Base16.decode . Char8.pack
-- >               $ str
-- >   builtindata :: Builtins.BuiltinData <- mapLeft (mappend "Cbor deserialization failed: " . show)
-- >     $ Codec.Serialise.deserialiseOrFail $ ByteString.Lazy.fromStrict binary
-- >   maybe (Left "'fromBuiltinData' for root hash of merkle tree failed") Right $
-- >     fromBuiltinData builtindata

-- | Decode a hexadecimal string into a BuiltinByteString
decodeHash :: Parser Text -> Parser Builtins.BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed ->
    either (const mzero) (pure . Builtins.toBuiltin) (tryDecode parsed)

-- * CLI flag parsers.

-- | 'initCommitteePublicKeysParser' is essentially identical to
-- 'newCommitteePublicKeysParser' except the help strings / command line flag
-- is changed to reflect that this is the inital committee.
initCommitteePublicKeysParser :: OptParse.Parser (IO [EcdsaSecp256k1PubKey])
initCommitteePublicKeysParser =
  fmap pure {- need to introduce io monad -} manyCommitteePublicKeys
    OptParse.<|> newCommitteeFile
  where
    manyCommitteePublicKeys =
      many
        $ option parseSidechainPubKey
        $ mconcat
          [ long "committee-pub-key"
          , metavar "PUBLIC_KEY"
          , help
              "Secp256k1 public key of an initial committee member (hex DER encoded [33 bytes])"
          ]

    newCommitteeFile = do
      committeeFilepath <-
        option OptParse.str
          $ mconcat
            [ long "committee"
            , metavar "FILEPATH"
            , help "Filepath of JSON generated committee from `fresh-sidechain-committee`"
            ]

      pure
        $ Aeson.decodeFileStrict' committeeFilepath
        >>= \case
          Just (SidechainCommittee members) -> pure $ fmap scmPublicKey members
          Nothing ->
            ioError
              $ userError
              $ "Invalid JSON committee file at: "
              <> committeeFilepath

-- | CLI parser for gathering the 'SidechainParams'
sidechainParamsParser :: OptParse.Parser SidechainParams
sidechainParamsParser = do
  chainId <-
    option auto
      $ mconcat
        [ short 'i'
        , long "sidechain-id"
        , metavar "1"
        , help "Sidechain ID"
        ]

  genesisUtxo <-
    option parseTxOutRef
      $ mconcat
        [ short 'c'
        , long "genesis-committee-hash-utxo"
        , metavar "TX_ID#TX_IDX"
        , help "Input UTxO to be spent with the first committee hash setup"
        ]

  governanceAuthority <-
    option parseGovernanceAuthority
      $ mconcat
        [ short 'g'
        , long "governance-authority"
        , metavar "PUB_KEY_HASH"
        , help "Public key hash of governance authority"
        ]

  (thresholdNumerator, thresholdDenominator) <-
    option parseThreshold
      $ mconcat
        [ long "threshold"
        , metavar "UINT/UINT"
        , help "Threshold ratio for the required number of signatures"
        ]

  pure SidechainParams {..}

signingKeyFileParser :: OptParse.Parser FilePath
signingKeyFileParser =
  option OptParse.str
    $ mconcat
      [ long "payment-signing-key-file"
      , metavar "FILEPATH"
      , help "Path to the signing key file"
      ]

-- | 'genCliCommandHelperParser' factors out the parsing of 'SidechainParams'
-- and the signing key file which is common to the parsers that generate a
-- subcommand
genCliCommandHelperParser :: OptParse.Parser (GenCliCommand -> Command)
genCliCommandHelperParser = do
  scParams <- sidechainParamsParser
  signingKeyFile <- signingKeyFileParser
  pure $ \cmd ->
    GenCliCommand
      { gccSidechainParams = scParams
      , gccSigningKeyFile = signingKeyFile
      , gccCliCommand = cmd
      }

-- * Commands for the subparsers

-- | 'registerCommand' is the subparser for gathering the parameters for
-- 'RegistrationCommand'.
registerCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
registerCommand =
  command "register"
    $ flip info (progDesc "Generates signatures for registering an spo")
    $ do
      scParamsAndSigningKeyFunction <- genCliCommandHelperParser
      rcSpoPrivKey <-
        do
          option parseSpoPrivKey
            $ mconcat
              [ long "spo-signing-key"
              , metavar "SIGNING_KEY"
              , help "SPO Cold signing key of the block producer candidate"
              ]
          OptParse.<|> do
            option parseSpoPrivKeyCbor
              $ mconcat
                [ long "spo-signing-key-cbor"
                , metavar "SIGNING_KEY_CBOR"
                , help "hex encoded cbor SPO signing key"
                ]

      rcSidechainPrivKey <-
        option parseSidechainPrivKey
          $ mconcat
            [ long "sidechain-signing-key"
            , metavar "SIGNING_KEY"
            , help "Signing key of the sidechain block producer candidate"
            ]

      rcRegistrationUtxo <-
        option parseTxOutRef
          $ mconcat
            [ long "registration-utxo"
            , metavar "TX_ID#TX_IDX"
            , help
                "Input UTxO to be spend with the commitee candidate registration"
            ]

      pure $ pure (scParamsAndSigningKeyFunction $ RegistrationCommand {..})
    <**> helper

deregisterCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
deregisterCommand =
  command "deregister"
    $ flip info (progDesc "Thin wrapper around `deregister` for deregistering an spo")
    $ do
      scParamsAndSigningKeyFunction <- genCliCommandHelperParser
      drSpoPubKey <-
        option parseSpoPubKeyCbor
          $ mconcat
            [ long "spo-pub-key-cbor"
            , metavar "PUB_KEY_CBOR"
            , help "Hex encoded cbor SPO public key"
            ]

      pure $ pure (scParamsAndSigningKeyFunction $ DeregistrationCommand {..})
    <**> helper

-- | 'initSidechainCommand' parses the cli arguments for gathering the
-- parameters for initalizing the sidechain
initSidechainCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
initSidechainCommand =
  command "init"
    $ flip
      info
      (progDesc "Generates the CLI command to initialize the sidechain")
    $ do
      scParamsAndSigningKeyFunction <- genCliCommandHelperParser

      ioIscInitCommitteePubKeys <- initCommitteePublicKeysParser

      -- mostly duplicated from 'updateCommitteeHashCommand'
      iscSidechainEpoch <-
        option auto
          $ mconcat
            [ long "sidechain-epoch"
            , metavar "INTEGER"
            , help "Sidechain epoch of the initial committee"
            ]
      pure $ do
        iscInitCommitteePubKeys <- ioIscInitCommitteePubKeys
        pure $ scParamsAndSigningKeyFunction $ InitSidechainCommand {..}
    <**> helper

-- | 'freshSidechainPrivateKeyCommand' parses the cli arguments to generate a
-- fresh sidechain private key
freshSidechainPrivateKeyCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
freshSidechainPrivateKeyCommand =
  command "fresh-sidechain-private-key"
    $ flip
      info
      (progDesc "Generates a fresh hex encoded sidechain private key")
    $ do
      pure $ pure $ SidechainKeyCommand FreshSidechainPrivateKey
    <**> helper

-- | 'freshSidechainCommittee' parses the cli arguments for generating a Json file
-- output of a sidechain committee
freshSidechainCommittee :: OptParse.Mod OptParse.CommandFields (IO Command)
freshSidechainCommittee = do
  command "fresh-sidechain-committee"
    $ flip
      info
      (progDesc "Generates a fresh sidechain committee in JSON format to stdout of a determined size")
    $ do
      fscCommitteeSize <-
        option (eitherReader nonNegativeIntParser)
          $ mconcat
            [ long "size"
            , metavar "INT"
            , help "Non-negative int to determine the size of the sidechain committee"
            ]
      pure $ pure $ SidechainKeyCommand FreshSidechainCommittee {..}
    <**> helper
  where
    nonNegativeIntParser :: HString.String -> Either HString.String Int
    nonNegativeIntParser =
      let go acc a =
            case acc of
              Left _ -> acc -- already an error, so give up..
              Right acc'
                -- we know the next "iteration" will multiply by 10,
                -- and add at most 9, so if `acc'` is greater or equal to this, then
                -- we know for sure that we'll go out of bounds..
                | acc' >= ((maxBound :: Int) `quot` 10) + 9 -> Left "Committee size too large"
                --  do the usual C magic trick (subtract by ascii value
                --  of @0@) to figure out what int that a digit is.
                | Char.isDigit a -> Right (acc' * 10 + (Char.ord a - Char.ord '0'))
                | otherwise -> Left "Invalid character"
       in List.foldl' go (Right 0)

-- | 'freshSidechainPrivateKeyCommand' parses the cli arguments to generate a
-- fresh sidechain private key
sidechainPrivateKeyToPublicKeyCommand ::
  OptParse.Mod OptParse.CommandFields (IO Command)
sidechainPrivateKeyToPublicKeyCommand =
  command "sidechain-private-key-to-public-key"
    $ flip
      info
      (progDesc "Computes the corresponding public key to a given private key")
    $ do
      spktpkPrivateKey <-
        option parseSidechainPrivKey
          $ mconcat
            [ long "sidechain-signing-key"
            , metavar "SIGNING_KEY"
            , help "Signing key of the sidechain block producer candidate"
            ]
      pure $ pure $ SidechainKeyCommand $ SidechainPrivateKeyToPublicKey {..}
    <**> helper

tryDecode :: Text -> Either HString.String ByteString
tryDecode = Base16.decode . encodeUtf8
